//--------------------------------------
//
// Parser.scala
// Since: 2012/08/16 11:50 AM
//
//--------------------------------------

package xerial.core.io.text.parser

import xerial.core.io.text.parser.Grammar.{ExprRef, Expr}
import xerial.core.io.text.Scanner
import annotation.tailrec
import xerial.core.log.Logger
import util.DynamicVariable
import xerial.lens.ObjectBuilder


sealed abstract class ParseError extends Exception
case object NoMatch extends ParseError
case class SyntaxError(message: String) extends ParseError


object Parser extends Logger {

  sealed abstract class ParseTree {
    def ~(t: ParseTree): ParseTree
  }

  case object Empty extends ParseTree {
    def ~(t: ParseTree) = Empty // TODO
  }

//  case class Token(t: Int) extends ParseTree {
//    def ~(t: ParseTree) = Empty // TODO
//  }

  case object OK extends ParseTree {
    def ~(t: ParseTree) = Empty // TODO
  }


  case class MatchedObject(obj:Any) extends ParseTree {
    def ~(t: ParseTree) = Empty // TODO
  }

  type ParseResult = Either[ParseError, ParseTree]


  class ParsingContext(val exprName:String, val builder:ObjectBuilder[_]) {
    def alias(n:String) : ParsingContext = new ParsingContext(n, builder)
    def set(value:Any)  {
      debug("set %s := %s", exprName, value)
      builder.set(exprName, value)
    }

  }



  abstract class Eval {
    def eval(name:String, resultType:Class[_]) : ParseResult = {
      val b = ObjectBuilder(resultType)
      val r = this.eval(new ParsingContext(name, b))
      r.right map { m =>
        MatchedObject(b.build)
      }
    }
    def eval(context:ParsingContext) :  ParseResult
  }


}






import Parser._

/**
 * @author leo
 */
class Parser(input: Scanner, e: ExprRef[_], ignoredExprs: Set[Expr]) extends Logger {

  import Grammar._

  private val body = build(e)
  private lazy val ignored = EvalOr("ignored", (ignoredExprs map { build(_) }).toArray[Eval])


  private def build(expr: Expr): Eval = {
    val cache = collection.mutable.Map[String, Eval]()

    def toEval(e: Expr): Eval = {
      if(cache.contains(e.name))
        cache(e.name)
      else {
        //debug("bulid %s", e)
        val ref = EvalRef(null)
        // Register a proxy entry to avoid recursive call of toEval
        cache += e.name -> ref
        val newEV: Eval = e match {
          case SeqNode(seq) => EvalSeq(e.name, seq map {
            toEval(_)
          })
          case OrNode(seq) => EvalOr(e.name, seq map {
            toEval(_)
          })
          case ExprRef(n, ref, rt) => EvalObj(n, toEval(ref), rt)
          case Alias(alias, expr) => EvalAlias(alias, toEval(expr))
          case Not(expr) => EvalNot(toEval(expr))
          case SyntacticPredicateFail(predToFail, expr) => EvalSyntacticPredicateFail(toEval(predToFail), toEval(expr))
          case Leaf(name, tt) => EvalCharPred(name, {t: Int =>
            t == tt
          })
          case CharPred(_, pred) => EvalCharPred(e.name, pred)
          case r@CharRange(_, _) => EvalCharPred(r.name, r.pred)
          case ZeroOrMore(a) => EvalZeroOrMore(toEval(a))
          case r@OneOrMore(a) => toEval(r.expr)
          case OptionNode(a) => EvalOption(toEval(a))
          case r@Repeat(a, sep) => toEval(r.expr)
        }
        ref.e = newEV
        newEV
      }
    }

    toEval(expr)
  }



  case class EvalRef(var e:Eval) extends Eval {
    def eval(context:ParsingContext) : ParseResult = e.eval(context)
  }


  case class EvalObj(name:String, e:Eval, resultType:Class[_]) extends Eval {
    def eval(context:ParsingContext) : ParseResult = {
      debug("eval %s in %s", e, name)
      e.eval(name, resultType)
    }
  }


  case class EvalAlias(alias:String, e:Eval) extends Eval {
    def eval(context:ParsingContext) : ParseResult = {
      e.eval(context.alias(alias))
    }
  }

  case class EvalNot(e: Eval) extends Eval {
    def eval(context:ParsingContext): ParseResult = {
      input.withMark {
        e.eval(context) match {
          case Left(_) => input.consume; Right(OK)
          case Right(_) => input.rewind; Left(NoMatch)
        }
      }
    }
  }

  case class EvalSyntacticPredicateFail(predToFail: Eval, e: Eval) extends Eval {
    def eval(context:ParsingContext): ParseResult = {
      input.withMark {
        noIgnore(predToFail.eval(context)) match {
          case Left(NoMatch) => input.rewind; e.eval(context)
          case other => input.rewind; Left(NoMatch)
        }
      }
    }
  }

  case class EvalSeq(name:String, seq: Array[Eval]) extends Eval {
    def eval(context:ParsingContext): ParseResult = {
      debug("EvalSeq %s", toVisibleString(name))
      @tailrec
      def loop(i: Int, t: ParseTree): ParseResult = {
        if(i >= seq.length)
          Right(t)
        else
          seq(i).eval(context) match {
            case l@Left(_) => l
            case r@Right(cc) => loop(i + 1, t ~ cc)
          }
      }
      loop(0, Empty)
    }
  }

  case class EvalOr(name:String, seq: Array[Eval]) extends Eval {
    def eval(context:ParsingContext): ParseResult = {

      debug("EvalOr %s", toVisibleString(name))

      @tailrec
      def loop(i: Int, t: ParseTree): ParseResult = {
        if(i >= seq.length) {
          evalIgnored(context) match {
            case l@ Left(_) => l
            case r@ Right(_) => loop(0, Empty)
          }
        }
        else
          seq(i).eval(context) match {
            case Left(NoMatch) => loop(i + 1, t)
            case other => other
          }
      }
      loop(0, Empty)
    }
  }

  private val fallbackToIgnoredToken = new DynamicVariable[Boolean](true)

  private def noIgnore(f: => ParseResult) : ParseResult = {
    val prev = fallbackToIgnoredToken.value
    fallbackToIgnoredToken.value = false
    try
      f
    finally
      fallbackToIgnoredToken.value = prev
  }

  /*
   * lookup ignored tokens
   */
  private def evalIgnored(context:ParsingContext): ParseResult = {
    if(fallbackToIgnoredToken.value) {
      trace("Eval ignored token: %s", input.first.toChar)
      input.withMark {
        noIgnore(ignored.eval(context)) match {
          case nm@Left(NoMatch) => nm
          case Right(_) => Right(OK)
          case other => other
        }
      }
    }
    else
      Left(NoMatch)
  }

  case class EvalCharPred(name:String, pred: Int => Boolean) extends Eval {
    def eval(context:ParsingContext): ParseResult = {
      @tailrec
      def loop(matchCount:Int) : ParseResult = {
        def exit = if(matchCount == 0) Left(NoMatch) else Right(OK)
        val t = input.first
        //debug("eval char pred %s: %s", Grammar.toVisibleString(name), Grammar.toVisibleString(t.toChar.toString))
        if (t == input.EOF)
          exit
        else if(pred(t)) {
          trace("match %s", t.toChar)
          input.consume
          loop(matchCount+1)
        }
        else
          exit
      }
      debug("eval char pred: %s", name)
      input.withMark {
        val r = loop(0)
        r.right.foreach { m =>
          context.set(input.selected)
        }
        r
      }
    }
  }

  case class EvalZeroOrMore(a: Eval) extends Eval {
    def eval(context:ParsingContext): ParseResult = {
      @tailrec def loop(t: ParseTree): ParseResult = {
        a.eval(context) match {
          case Left(NoMatch) => Right(t)
          case l@Left(_) => l
          case Right(next) => loop(t ~ next)
        }
      }
      loop(Empty)
    }
  }


  case class EvalOption(a: Eval) extends Eval {
    def eval(context:ParsingContext): ParseResult = {
      a.eval(context) match {
        case Left(NoMatch) => Right(OK)
        case other => other
      }
    }
  }


  def parse = {
    debug("parse %s", body)
    body.eval(new ParsingContext(null, null))
  }

}