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
 * Toke type
 */
trait TokenType {
  val name : String = this.getClass.getSimpleName.replaceAll("""\$""", "")
  override def toString = name
}

/**
 * Symbolic token
 * @param symbol
 */
abstract class SymbolToken(val symbol:String) extends Token with TokenType {
  def tokenType = this
}


/**
 * Token definition
 */
trait Token {
  def tokenType : TokenType
}


sealed abstract class ParseError extends Exception
case object NoMatch extends ParseError
case class SyntaxError(message: String) extends ParseError

trait Grammar extends Logging {
  import Grammar._

  implicit def toExpr(t:TokenType) : Expr = new Leaf(t)

  def repeat(expr: Expr, separator: TokenType): Expr = Repeat(expr, separator)
  def oneOrMore(expr: Expr, separator: TokenType) : Expr = (expr ~ ZeroOrMore(Leaf(separator) ~ expr))
  def option(expr: Expr): Expr = OptionNode(expr)

  def rule(expr: => Expr): Expr = rule(getEnclosingRuleName(3), expr)

  /**
   * Rule construction method. The body expression is called-by-name to enable recursive rule definitions, e.g.,
   *
   * <code>
   * def expr = ("(" ~ expr ~ ")") | Int
   * </code>
   *
   * @param expr
   * @return
   */
  def rule(ruleName:String, expr: => Expr) : Expr = {
    ruleCache.get(ruleName) match {
      case Some(r) => r
      case None => {

        // Insert a proxy entry to avoid recursively calling this method
        val proxy = ExprProxy(ruleName, null)
        ruleCache += ruleName -> proxy
        // Prepare the rule
        val newExpr : Expr = expr
        ruleCache += ruleName -> newExpr
        // Materialize the proxy in case it is called
        proxy.set(newExpr)
        debug("Define rule %s := %s", ruleName, newExpr)
        newExpr
      }
    }
  }

  private var ruleCache : Map[String, Expr] = Map[String, Expr]()

  private def getEnclosingRuleName(stackLevel:Int) : String = {
    new Throwable().getStackTrace()(stackLevel).getMethodName
  }
}


/**
 * Syntax grammar
 *
 * @author leo
 */
object Grammar extends Logging {

  type ParseResult = Either[ParseError, Parser]

  trait Parser {
    def LA1: Token
    def consume: Parser
    def getRule(name:String) : Expr
    def firstTokenTypeOf(tree:Expr) : Seq[TokenType]
  }

  /**
   * Parsing expression
   * @param name
   */
  sealed abstract class Expr(val name:String) { a : Expr =>
    def ~(b: Expr) : Expr = SeqNode(Array(a, b))
    def |(b: Expr) : Expr = OrNode(Array(a, b))
    def eval(in:Parser) : ParseResult
    override def toString = name
  }

  case class ExprProxy(override val name:String, private var expr:Expr)  extends Expr(name) {
    def eval(in:Parser) = expr.eval(in)
    private[Grammar] def set(newExpr:Expr) { expr = newExpr }
  }

  case class ExprRef(override val name:String) extends Expr(name) {
    def eval(in: Parser) = {
      trace("eval %s", name)
      val t = in.getRule(name)
      t.eval(in)
    }
  }

  case class Leaf(tt: TokenType) extends Expr(tt.name) {
    def eval(in: Parser) = {
      val t = in.LA1
      trace("eval %s, LA1:%s", tt, t)
      if (t.tokenType == tt) {
        debug("match %s, LA1:%s", t.tokenType, t)
        Right(in.consume)
      }
      else
        Left(NoMatch)
    }
  }


  case class OrNode(seq:Array[Expr]) extends Expr(seq.map(_.name).mkString(" | ")) {
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
      loop(0, lookupTable(in).getOrElse(t.tokenType, seq), in)
    }

  }

  case class SeqNode(seq:Array[Expr]) extends Expr(seq.map(_.name).mkString(" ")) {
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

  case class ZeroOrMore(a: Expr) extends Expr("(%s)*".format(a.name)) {
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
  case class OptionNode(a: Expr) extends Expr("(%s)?".format(a.name)) {
    def eval(in: Parser) = {
      a.eval(in) match {
        case l@Left(NoMatch) => Right(in)
        case other => other
      }
    }
  }


  case class Repeat(a:Expr, separator:TokenType) extends Expr("rep(%s,%s)".format(a.name, separator.name)) {
    private val p = OptionNode(a ~ ZeroOrMore(Leaf(separator) ~ a))
    def eval(in: Parser) = p.eval(in)
  }


}
