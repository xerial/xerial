//--------------------------------------
//
// Parser.scala
// Since: 2012/08/16 11:50 AM
//
//--------------------------------------

package xerial.core.io.text.parser

import xerial.core.io.text.parser.Grammar.{SeqNode, Expr}
import xerial.core.io.text.Scanner
import annotation.tailrec
import xerial.core.log.Logging


sealed abstract class ParseError extends Exception
case object NoMatch extends ParseError
case class SyntaxError(message: String) extends ParseError


object Parser {

  sealed abstract class ParseTree {
    def ~(t: ParseTree): ParseTree
  }

  case object Empty extends ParseTree {
    def ~(t: ParseTree) = Empty // TODO
  }

  case class Token(t: Int) extends ParseTree {
    def ~(t: ParseTree) = Empty // TODO
  }

  case object OK extends ParseTree {
    def ~(t: ParseTree) = Empty // TODO
  }


  type ParseResult = Either[ParseError, ParseTree]

  case class ParsingContext(tree: ParseTree)

  abstract class Eval {
    def eval: ParseResult
  }
}

import Parser._

/**
 * @author leo
 */
class Parser(input: Scanner, e: Expr, ignoredExprs: Set[Expr]) extends Logging {

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
          case ExprRef(_, ref) => toEval(ref)
          case Not(expr) => EvalNot(toEval(expr))
          case SyntacticPredicateFail(predToFail, expr) => EvalSyntacticPredicateFail(toEval(predToFail), toEval(expr))
          case Leaf(name, tt) => EvalCharPred(name, {t: Int =>
            t == tt
          })
          case CharPred(_, pred) => EvalCharPred(e.name, pred)
          case r@CharRange(_, _) => EvalCharPred(r.name, r.pred)
          case ZeroOrMore(a) => EvalZeroOrMore(toEval(a))
          case OneOrMore(a) => EvalOneOrMore(toEval(a))
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
    def eval : ParseResult = e.eval
  }

  case class EvalNot(e: Eval) extends Eval {
    def eval: ParseResult = {
      input.withMark {
        e.eval match {
          case Left(_) => input.consume; Right(OK)
          case Right(_) => input.rewind; Left(NoMatch)
        }
      }
    }
  }

  case class EvalSyntacticPredicateFail(predToFail: Eval, e: Eval) extends Eval {
    def eval: ParseResult = {
      input.withMark {
        predToFail.eval match {
          case Left(NoMatch) => input.rewind; e.eval
          case other => input.rewind; Left(NoMatch)
        }
      }
    }
  }

  case class EvalSeq(name:String, seq: Array[Eval]) extends Eval {
    def eval: ParseResult = {
      debug("eval seq %s", name)
      @tailrec
      def loop(i: Int, t: ParseTree): ParseResult = {
        if(i >= seq.length)
          Right(t)
        else
          seq(i).eval match {
            case l@Left(_) => l
            case r@Right(cc) => loop(i + 1, t ~ cc)
          }
      }
      loop(0, Empty)
    }
  }

  case class EvalOr(name:String, seq: Array[Eval]) extends Eval {
    def eval: ParseResult = {

      debug("eval or %s", name)

      @tailrec
      def loop(i: Int, t: ParseTree): ParseResult = {
        if(i >= seq.length) {
          debug("no match for %s", name)
          Left(NoMatch)
        }
        else
          seq(i).eval match {
            case Left(NoMatch) => loop(i + 1, t)
            case other => other
          }
      }
      loop(0, Empty)
    }
  }

  /*
   * lookup ignored tokens
   */
  private def evalIgnored: ParseResult = {
    debug("eval ignored: %s", input.first.toChar)
    input.withMark {
      ignored.eval match {
        case nm@Left(NoMatch) => input.rewind; nm
        case Right(_) => input.consume; Right(OK)
        case other => other
      }
    }
  }

  case class EvalCharPred(name:String, pred: Int => Boolean) extends Eval {
    def eval: ParseResult = {
      input.withMark {
        val t = input.first
        debug("eval char pred %s: %s", name, t.toChar)
        if (t != input.EOF && pred(t)) {
          trace("match %s", t.toChar)
          input.consume
          Right(OK)
        }
        else
          Left(NoMatch)
      }
    }
  }

  case class EvalZeroOrMore(a: Eval) extends Eval {
    def eval: ParseResult = {
      @tailrec def loop(t: ParseTree): ParseResult = {
        a.eval match {
          case Left(NoMatch) => Right(t)
          case l@Left(_) => l
          case Right(next) => loop(t ~ next)
        }
      }
      loop(Empty)
    }
  }
  case class EvalOneOrMore(a: Eval) extends Eval {
    def eval: ParseResult = {
      @tailrec def loop(i: Int, t: ParseTree): ParseResult = {
        a.eval match {
          case Left(NoMatch) if i > 0 => Right(t)
          case l@Left(_) => l
          case Right(next) => loop(i + 1, t ~ next)
        }
      }
      loop(0, Empty)
    }
  }

  case class EvalOption(a: Eval) extends Eval {
    def eval: ParseResult = {
      a.eval match {
        case Left(NoMatch) => Right(OK)
        case other => other
      }
    }
  }


  def parse = {
    @tailrec def loop : ParseResult = {
      debug("parse %s", body)
      body.eval match {
        case Left(NoMatch) => evalIgnored match {
          case l @ Left(_) => l
          case r @ Right(_) => loop
        }
        case other => other
      }
    }

    loop
  }

}