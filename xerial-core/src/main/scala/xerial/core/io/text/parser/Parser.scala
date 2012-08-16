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


sealed abstract class ParseError extends Exception
case object NoMatch extends ParseError
case class SyntaxError(message: String) extends ParseError


object Parser {

  sealed abstract class ParseTree {
    def ~(t:ParseTree) : ParseTree
  }

  case object Empty extends ParseTree {
    def ~(t: ParseTree) = null
  }

  case class Token(t:Int) extends ParseTree {
    def ~(t: ParseTree) = null
  }

  case object OK extends ParseTree {
    def ~(t: ParseTree) = null
  }


  type ParseResult = Either[ParseError, ParseTree]

  case class ParsingContext(tree:ParseTree)

}

import Parser._

/**
 * @author leo
 */
class Parser(input:Scanner, e:Expr, ignoredExprs:Seq[Expr]) {

  import Grammar._
  private val body = build(e)
  private val ignored = EvalOr(ignoredExprs map { build } toArray)

  def build(expr:Expr) : Eval = {
    def toEval(e:Expr) : Eval = {
      e match {
        case SeqNode(seq) => EvalSeq(seq map { toEval(_) })
        case OrNode(seq) => EvalOr(seq map { toEval(_) })
        case ExprRef(_, ref) => toEval(ref)
        case Not(expr) => EvalNot(toEval(expr))
        case SyntacticPredicateFail(predToFail, expr) => EvalSyntacticPredicateFail(toEval(predToFail), toEval(expr))
        case Leaf(name, tt) => EvalCharPred({t:Int => t == tt})
        case CharPred(_, pred) => EvalCharPred(pred)
        case r @ CharRange(_, _) => EvalCharPred(r.pred)
        case ZeroOrMore(a) => EvalZeroOrMore(toEval(a))
        case OneOrMore(a) => EvalOneOrMore(toEval(a))
        case OptionNode(a) => EvalOption(toEval(a))
        case r @ Repeat(a, sep) => toEval(r.expr)
      }
    }

    toEval(expr)
  }

  abstract class Eval {
    def eval : ParseResult
  }

  case class EvalNot(e:Eval) extends Eval {
    def eval : ParseResult = {
      input.mark
      e.eval match {
        case Left(_) => input.consume; Right(OK)
        case Right(_) => input.rewind; Left(NoMatch)
      }
    }
  }

  case class EvalSyntacticPredicateFail(predToFail:Eval, e:Eval) extends Eval {
    def eval : ParseResult = {
      input.withMark {
        predToFail.eval match {
          case Left(NoMatch) => e.eval
          case other => Left(NoMatch)
        }
      }
    }
    eval
  }

  case class EvalSeq(seq:Array[Eval]) extends Eval {
    def eval : ParseResult = {
      @tailrec
      def loop(i:Int, t:ParseTree) : ParseResult = {
        seq(i).eval match {
          case l@Left(_) => l
          case r@Right(cc) => loop(i+1, t ~ cc)
        }
      }
      loop(0, Empty)
    }
  }

  case class EvalOr(seq:Array[Eval]) extends Eval {
    def eval : ParseResult = {
      @tailrec
      def loop(i:Int, t:ParseTree) : ParseResult = {
        seq(i).eval match {
          case Left(NoMatch) => loop(i+1, t)
          case other => other
        }
      }
      loop(0, Empty)
    }
  }

  /*
   * lookup ignored tokens
   */
  private def evalIgnored : ParseResult = {
    input.mark
    @tailrec def loop : ParseResult = {
      ignored.eval match {
        case nm@Left(NoMatch) => { input.rewind; nm }
        case Right(_) => { input.consume; loop }
        case other => other
      }
    }
    loop
  }

  case class EvalCharPred(pred:Int => Boolean) extends Eval {
    def eval : ParseResult = {
      input.mark
      def loop : ParseResult = {
        val t = input.first
        if(pred(t)) {
          input.consume
          Right(OK)
        }
        else
          evalIgnored match {
            case Right(_) => loop
            case other => other
          }
      }
      loop
    }
  }

  case class EvalZeroOrMore(a:Eval) extends Eval {
    def eval : ParseResult = {
      @tailrec def loop(t:ParseTree): ParseResult = {
        a.eval match {
          case Left(NoMatch) => Right(t)
          case l@Left(_) => l
          case Right(next) => loop(t ~ next)
        }
      }
      loop(Empty)
    }
  }
  case class EvalOneOrMore(a:Eval) extends Eval {
    def eval : ParseResult = {
      @tailrec def loop(i:Int, t:ParseTree): ParseResult = {
        a.eval match {
          case Left(NoMatch) if i>0 => Right(t)
          case l@Left(_) => l
          case Right(next) => loop(i+1, t ~ next)
        }
      }
      loop(0, Empty)
    }
  }

  case class EvalOption(a:Eval) extends Eval {
    def eval : ParseResult = {
      a.eval match {
        case Left(NoMatch) => Right(OK)
        case other => other
      }
    }
  }



  def parse = {
    body.eval
  }

}