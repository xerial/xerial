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


  case class Value(name:String, value:Any)

  sealed abstract class ParseTree {
    def foreach[U](f: Value => U) : Unit
    def addSibling(t:ParseTree) : ParseTree
    def addChild(t:ParseTree) : ParseTree
  }


  case class Node(v:Value, child:ParseTree, sibling:ParseTree) extends ParseTree {
    def foreach[U](f: Value => U) {
      // depth-first search
      child.foreach(f)
      f(v)
      sibling.foreach(f)
    }
    def addSibling(t: ParseTree) = Node(v, child, sibling.addSibling(t))
    def addChild(t: ParseTree) = Node(v, child.addSibling(t), sibling)
  }

  case class Leaf(v:Value) extends ParseTree {
    def foreach[U](f: Value => U) { f(v) }
    def addSibling(t: ParseTree) = Node(v, Empty, t)
    def addChild(t: ParseTree) = Node(v, t, Empty)
  }

  case object Empty extends ParseTree {
    def foreach[U](f: Value => U) {
      // do nothing
    }
    def addSibling(t: ParseTree) = t
    def addChild(t: ParseTree) = t
  }

  type ParseResult = Either[ParseError, ParseTree]

  case class ParsingContext(exprName:String) {
    def alias(n:String) : ParsingContext = new ParsingContext(n)

    def newNode(v:Any) = Leaf(Value(exprName, v))
  }


  abstract class Eval {
    def eval(context:ParsingContext) :  ParseResult
  }


}






import Parser._

/**
 * @author leo
 */
class Parser(input: Scanner, e: ExprRef[_], ignoredExprs: Set[Expr]) extends Logger {

  private val body = build(e)
  private lazy val ignored = EvalOr("ignored", (ignoredExprs map { build(_) }).toArray[Eval])


  private def build(expr: Expr): Eval = {
    val cache = collection.mutable.Map[String, Eval]()

    import Grammar._

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
          case CharSetPred(name, cs) => EvalCharPred(e.name, cs.contains(_))
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
      debug("eval %s[%s] %s", Grammar.toVisibleString(name), resultType.getSimpleName, e.toString)
      val r = e.eval(new ParsingContext(name))
      val b = ObjectBuilder(resultType)
      r.right map { m =>
        debug("parse tree: %s", m)
        m.foreach(v =>
          b.set(v.name, v.value)
        )
        context.newNode(b.build)
      }
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
          case Left(_) => Right(Empty)
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
      debug("EvalSeq %s", Grammar.toVisibleString(name))
      @tailrec
      def loop(i: Int, t: ParseTree): ParseResult = {
        if(i >= seq.length) {
          //trace("eval seq end: %s", t)
          Right(t)
        }
        else
          seq(i).eval(context) match {
            case l@Left(_) => l
            case r@Right(cc) => loop(i + 1, t.addSibling(cc))
          }
      }
      val r = loop(0, Empty)
      //debug("eval seq result: %s", r)
      r
    }
  }

  case class EvalOr(name:String, seq: Array[Eval]) extends Eval {
    def eval(context:ParsingContext): ParseResult = {
      // TODO Use an automaton that looks next characters, then switches matching patterns
      @tailrec
      def loop(i: Int, t: ParseTree): ParseResult = {
        if(i >= seq.length) {
          Left(NoMatch)
//          // Uses pattern matching for making the code tail recursive.
//          evalIgnored(context) match {
//            case l@ Left(_) => l
//            case r@ Right(_) => loop(0, Empty)
//          }
        }
        else
          seq(i).eval(context) match {
            case Left(NoMatch) => loop(i + 1, t)
            case other => other
          }
      }
      debug("EvalOr %s, context:%s", Grammar.toVisibleString(name), context)
      loop(0, Empty)
    }
  }

  private val ignoredTokensEnabled = new DynamicVariable[Boolean](true)

  private def noIgnore(f: => ParseResult) : ParseResult = {
    val prev = ignoredTokensEnabled.value
    ignoredTokensEnabled.value = false
    try
      f
    finally
      ignoredTokensEnabled.value = prev
  }

  /*
   * lookup ignored tokens
   */
  private def evalIgnored(context:ParsingContext): ParseResult = {
    if(ignoredTokensEnabled.value) {
      trace("Eval ignored token: %s", input.first.toChar)
      input.withMark {
        val m = noIgnore(ignored.eval(context))
        trace("match : %s", m)
        m match {
          case nm@Left(NoMatch) => { input.rewind; nm } // non-ignorable pattern
          case Right(_) => Right(Empty) // matched to an ignored pattern
          case other => other // other error
        }
      }
    }
    else
      Left(NoMatch)
  }


  case class EvalCharPred(name:String, pred: Int => Boolean) extends Eval {
    def eval(context:ParsingContext): ParseResult = {

      def exit(matchCount:Int) : ParseResult = {
        if(matchCount == 0) {
          evalIgnored(context) match {
            case Right(_) => {
              //trace("ignore token: %s (next char:%s)", Grammar.toVisibleString(input.selected), input.first.toChar)
              // consume the input
              input.withMark {
                loop(0)
              }
            }
            case Left(_) => Left(NoMatch)
          }
        }
        else
          Right(context.newNode(input.selected))
      }

      @tailrec
      def loop(matchCount:Int) : ParseResult = {
        val t = input.first
        //debug("eval char pred %s: %s", Grammar.toVisibleString(name), Grammar.toVisibleString(t.toChar.toString))
        if (t == input.EOF || !pred(t))
          exit(matchCount)
        else {
          trace("match %s", t.toChar)
          input.consume
          loop(matchCount+1)
        }
      }
      debug("eval char pred: %s", Grammar.toVisibleString(name))
      // TODO need to distinguish repetitive match and single character match
      input.withMark { loop(0) }
    }
  }

  case class EvalZeroOrMore(a: Eval) extends Eval {
    def eval(context:ParsingContext): ParseResult = {
      @tailrec def loop(t: ParseTree): ParseResult = {
        a.eval(context) match {
          case Left(NoMatch) => Right(t)
          case l@Left(_) => l
          case Right(next) => loop(t.addSibling(next))
        }
      }
      loop(Empty)
    }
  }


  case class EvalOption(a: Eval) extends Eval {
    def eval(context:ParsingContext): ParseResult = {
      a.eval(context) match {
        case Left(NoMatch) => Right(Empty)
        case other => other
      }
    }
  }


  def parse = {
    debug("parse %s", body)
    body.eval(new ParsingContext(null))
  }

}