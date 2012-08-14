//--------------------------------------
//
// Grammar.scala
// Since: 2012/08/14 2:39 PM
//
//--------------------------------------

package xerial.core.io.text.parser

import xerial.core.log.Logging
import annotation.tailrec


/**
 * Token definition
 */
trait Token {

}

/**
 * Symbolic token
 */
class SymbolToken(val name:String, val symbol:String) extends Token {

}




sealed abstract class ParseError extends Exception
case object NoMatch extends ParseError
case class SyntaxError(message: String) extends ParseError

trait Grammar extends Logging {
  import Grammar._

  implicit def toToken(t:String) : Expr = new Leaf("'%s'".format(t), t.charAt(0))
  implicit def toParserExpr(a:String) = new {
    // convert to range
    def -(b:String) : Expr = CharRange(a, b)
    // Syntactic predicate without consumption of stream
    def !=>(expr: => Expr) : Expr = {
      val pred = toToken(a)
      SyntacticPredicateFail(pred, rule("!>%s".format(pred.hashCode), expr))
    }
  }

  def untilEOF : Expr = CharPred("untilEOF", { ch:Int => ch != -1 })
  def not(ch:String) : Expr = Not(toToken(ch))
  def repeat(expr: Expr, separator: Expr): Expr = Repeat(expr, separator)
  def repeat(expr: Expr): Expr = ZeroOrMore(expr)
  def oneOrMore(expr: Expr, separator: Expr) : Expr = (expr ~ ZeroOrMore(separator ~ expr))
  def option(expr: Expr): Expr = OptionNode(expr)


  def token(str:String) : Expr = {
    val tokenName = getEnclosingMethodName(3)
    ruleCache.get(tokenName) match {
      case Some(t) => t
      case None => {
        val l = Leaf(tokenName, str.charAt(0))
        ruleCache += tokenName -> l
        debug("Define token %14s := '%s'", tokenName, str)
        l
      }
    }
  }


  /**
   * Rule construction method. The body expression is called-by-name to enable recursive rule definitions, e.g.,
   *
   * <code>
   * def expr = rule { ("(" ~ expr ~ ")") | Int }
   * </code>
   *
   * @param expr
   * @return
   */
  def rule(expr: => Expr): Expr = rule(getEnclosingMethodName(3), expr)

  /**
   * Construct a new rule with a given name
   * @param ruleName
   * @param expr
   * @return
   */
  def rule(ruleName:String, expr: => Expr) : Expr = {
    ruleCache.get(ruleName) match {
      case Some(r) => r
      case None => {
        // Insert a reference to this rule first to avoid recursively calling this method
        val ref = ExprRef(ruleName, null)
        ruleCache += ruleName -> ref
        // Prepare the rule
        val newExpr : Expr = expr
        // Update the reference
        ref.set(newExpr)
        debug("Define rule %15s := %s", ruleName, newExpr)
        ref
      }
    }
  }

  private var ruleCache : Map[String, Expr] = Map[String, Expr]()

  private def getEnclosingMethodName(stackLevel:Int) : String = {
    new Throwable().getStackTrace()(stackLevel).getMethodName
  }
}


/**
 * Syntax grammar
 *
 * @author leo
 */
object Grammar extends Logging {

  type TokenType = Int

  type ParseResult = Either[ParseError, Parser]

  trait Parser {
    def LA1: Int
    def consume: Parser
    def getRule(name:String) : Expr
    def firstTokenTypeOf(tree:Expr) : Seq[TokenType]
  }


  def toVisibleString(s: CharSequence): String = {
    if (s == null) return ""
    var text: String = s.toString
    text = text.replaceAll("\n", "\\\\n")
    text = text.replaceAll("\r", "\\\\r")
    text = text.replaceAll("\t", "\\\\t")
    text
  }

  /**
   * Parsing expression
   * @param name
   */
  sealed abstract class Expr(val name:String) { a : Expr =>
    def ~(b: Expr) : Expr = SeqNode(Array(a, b))
    def |(b: Expr) : Expr = OrNode(Array(a, b))
    def or(b: Expr) : Expr = OrNode(Array(a, b))
    def + : Expr = OneOrMore(a)
    def * : Expr = ZeroOrMore(a)
    def eval(in:Parser) : ParseResult
    override def toString = toVisibleString(name)
  }

  case class ExprRef(override val name:String, private var expr:Expr)  extends Expr(name) {
    def eval(in:Parser) = expr.eval(in)
    private[Grammar] def set(newExpr:Expr) { expr = newExpr }
  }

  case class SyntacticPredicateFail(predToFail:Expr, e:Expr) extends Expr("(%s) !=> %s".format(predToFail, e)) {
    def eval(in: Parser) = null // TODO
  }

  case class Not(e:Expr) extends Expr("!(%s)".format(e)) {
    def eval(in: Parser) = null // TODO
  }

  case class CharRange(a:String, b:String) extends Expr("[%s-%s]".format(a, b)) {
    require(a.length == 1)
    require(b.length == 1)
    def eval(in: Parser) = null // TODO
  }

  case class CharPred(override val name:String, pred: Int => Boolean) extends Expr(name) {
    def eval(in: Parser) = {
      @tailrec
      def loop {
        val c = in.LA1
        if(c != -1 && pred(c)) {
          in.consume
          loop
        }
      }
      loop
      Right(in)
    }
  }


  case class Leaf(override val name:String, tt: TokenType) extends Expr(name) {
    def eval(in: Parser) = {
      val t = in.LA1
      trace("eval %s, LA1:%s", tt, t)
      if (t == tt) {
        debug("match %s", t)
        Right(in.consume)
      }
      else
        Left(NoMatch)
    }
  }

  case class OrNode(seq:Array[Expr]) extends Expr("(%s)".format(seq.map(_.name).mkString(" | "))) {
    override def |(b: Expr) : Expr = OrNode(seq :+ b)

    var table : Map[TokenType, Array[Expr]] = null

    private def lookupTable(p:Parser) : Map[TokenType, Array[Expr]] = {
      if(table == null) {
        val tokenToTree = for((tree, index) <- seq.zipWithIndex; tt <- p.firstTokenTypeOf(tree)) yield tt -> index
        val entries = for((token, pairSeq) <- tokenToTree.groupBy(_._1)) yield {
          val indexes = pairSeq.map(_._2).distinct.sorted.map(seq(_)).toArray
          token -> indexes
        }
        table = entries.toMap
      }
      table
    }

    def eval(in: Parser) = {
      @tailrec def loop(i:Int, lst:Array[Expr], p:Parser) : ParseResult = {
        if(i >= lst.length)
          Right(p)
        else {
          lst(i).eval(p) match {
            case Left(NoMatch) => loop(i+1, lst, p)
            case other => other
          }
        }
      }

      trace("eval %s", name)
      val t = in.LA1
      loop(0, lookupTable(in).getOrElse(t, seq), in)
    }

  }

  case class SeqNode(seq:Array[Expr]) extends Expr("(%s)".format(seq.map(_.name).mkString(" "))) {
    override def ~(b: Expr) : Expr = SeqNode(seq :+ b)
    def eval(in:Parser) = {
      @tailrec def loop(i:Int, p:Parser) : ParseResult = {
        if(i >= seq.length)
          Right(p)
        else {
          seq(i).eval(in) match {
            case l@Left(_) => l
            case Right(next) => loop(i+1, next)
          }
        }
      }
      loop(0, in)
    }
  }

  case class ZeroOrMore(a: Expr) extends Expr("%s*".format(a.name)) {
    def eval(in:Parser) = {
      @tailrec def loop(p: Parser): ParseResult = {
        a.eval(p) match {
          case Left(NoMatch) => Right(p)
          case l@Left(_) => l
          case Right(next) => loop(next)
        }
      }
      loop(in)
    }
  }

  case class OneOrMore(a: Expr) extends Expr("%s+".format(a.name)) {
    def eval(in:Parser) = {
      @tailrec def loop(p: Parser, count:Int): ParseResult = {
        a.eval(p) match {
          case Left(NoMatch) if count > 0 => Right(p)
          case l@Left(_) => l
          case Right(next) => loop(next, count+1)
        }
      }
      loop(in, 0)
    }
  }

  case class OptionNode(a: Expr) extends Expr("%s?".format(a.name)) {
    def eval(in: Parser) = {
      a.eval(in) match {
        case l@Left(NoMatch) => Right(in)
        case other => other
      }
    }
  }


  case class Repeat(a:Expr, separator:Expr) extends Expr("rep(%s,%s)".format(a.name, separator.name)) {
    private val p = OptionNode(a ~ ZeroOrMore(separator ~ a))
    def eval(in: Parser) = p.eval(in)
  }


}
