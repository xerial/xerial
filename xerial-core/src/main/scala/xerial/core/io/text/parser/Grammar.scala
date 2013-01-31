/*
 * Copyright 2012 Taro L. Saito
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//--------------------------------------
//
// Grammar.scala
// Since: 2012/08/14 2:39 PM
//
//--------------------------------------

package xerial.core.io.text.parser

import xerial.core.log.Logger
import annotation.tailrec
import xerial.core.io.text.StringScanner
import scala.language.implicitConversions



/**
 * A trait for defining expression grammars.
 *
 * <h3>Example:</h3>
 * <code>
 * <pre>
 * trait MyGrammar extends Grammar {
 *
 * // Define expression patterns using expr blocks
 * // '~' denotes a sequence of matches
 * def comment = expr { "#" ~ untilEOF }
 * def expr  = expr { value | string | "(" ~ expr ~ ")" }
 * def value = expr { "0" - "9" | "A" - "Z" | "a" - "z" } // Range of characters
 *
 * // repetition of patterns and syntactic predicate (!->).
 * def string     = expr { "\"" ~ repeat("\\" !->  not("\"") | escapeSequence) ~ "\"" }
 * def escapeSequence = expr { "\\" ~ ("\"" | "\\" | "/" | "b" | "f" | "n" | "r" | "t" | "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit) }
 * def digit      = expr { "0" - "9" }
 * def hexDigit   = expr { digit | "A" - "F" | "a" - "f" }
 *
 * // Define tokens to ignore
 * def whiteSpace = expr { " " | "\t" | "\n" | "\r" }
 * ignore(whiteSpace)
 * }
 * </pre>
 * </code>
 *
 *
 */
trait Grammar extends Logger {

  import Grammar._

  implicit def toToken(t: String): Expr = new Leaf("'%s'".format(t), t.charAt(0).toInt)

  implicit def toParserExpr(a: String) = new {
    // convert to range
    def -(b: String): Expr = CharRange(a, b)

    // Syntactic predicate without consumption of stream
    def !->(expr: => Expr): Expr = {
      val pred = toToken(a)
      SyntacticPredicateFail(pred, rule(newNonDuplicateRuleID("Then"), expr))
    }
  }

  def untilEOF: Expr = CharPred("<untilEOF>", {
    ch: Int => ch != -1
  })

  def not(ch: String): Expr = Not(toToken(ch))
  def repeat(expr: Expr, separator: Expr): Expr = Repeat(expr, separator)
  def repeat(expr: Expr): Expr = ZeroOrMore(expr)
  def zeroOrMore(expr: Expr): Expr = repeat(expr)
  def oneOrMore(expr: Expr, separator: Expr): Expr = (expr ~ ZeroOrMore(separator ~ expr))
  def oneOrMore(expr: Expr): Expr = OneOrMore(expr)
  def option(expr: Expr): Expr = OptionNode(expr)

  /**
   * Construct a token expr from a string
   * @param str
   * @return
   */
  def token(str: String): Expr = {
    require(str.length == 1, "token string be a single character")
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
   * Construct an expression. The expression is called-by-name to enable recursive definitions, e.g.,
   *
   * <code>
   * def expr = expr { ("(" ~ expr ~ ")") | Int }
   * </code>
   *
   * @param expr
   * @return
   */
  def expr(expr: => Expr): Expr = rule(getEnclosingMethodName(3), expr)

  /**
   * Add an ignored expr
   * @param rules
   */
  def ignore(rules: Expr*): Unit = {
    rules foreach {
      r =>
        debug("Tokens that match the expr %s will be ignored", r.name)
        ignoredExprs += r
    }
  }


  /**
   * Construct a new expr with a given name
   * @param ruleName
   * @param expr
   * @return
   */
  def rule(ruleName: String, expr: => Expr): Expr = {
    ruleCache.get(ruleName) match {
      case Some(r) => r
      case None => {
        // Insert a reference to this expr first to avoid recursively calling this method
        val ref = ExprRef(ruleName, null)
        ruleCache += ruleName -> ref
        // Prepare the expr
        val newExpr: Expr = expr
        // Update the reference
        ref.set(newExpr)
        debug("Define expr %15s := %s", ruleName, newExpr)
        ref
      }
    }
  }

  private var ruleCache: Map[String, Expr] = Map[String, Expr]()
  private val prefixCount = collection.mutable.Map[String, Int]()

  private def newNonDuplicateRuleID(prefix: String): String = {
    val count = prefixCount.getOrElseUpdate(prefix, 0)
    prefixCount += prefix -> (count + 1)
    "%s%d".format(prefix, count + 1)
  }

  private var ignoredExprs: Set[Expr] = Set()

  private def getEnclosingMethodName(stackLevel: Int): String = {
    new Throwable().getStackTrace()(stackLevel).getMethodName
  }

  def parse(e:Expr, s:String) = {
    trace("preparing parser")
    val p = new Parser(new StringScanner(s), e, ignoredExprs)
    trace("parse start")
    p.parse
  }
}


/**
 * Syntax grammar
 *
 * @author leo
 */
object Grammar extends Logger {


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
  sealed abstract class Expr(val name: String) { a: Expr =>
    def ~(b: Expr): Expr = SeqNode(Array(a, b))
    def |(b: Expr): Expr = OrNode(Array(a, b))
    def or(b: Expr): Expr = OrNode(Array(a, b))

    override def toString = toVisibleString(name)
  }

  case class OrNode(seq: Array[Expr]) extends Expr("(%s)".format(seq.map(_.name).mkString(" | "))) {
    override def |(b: Expr): Expr = OrNode(seq :+ b)
  }
  case class SeqNode(seq: Array[Expr]) extends Expr("(%s)".format(seq.map(_.name).mkString(" "))) {
    override def ~(b: Expr): Expr = SeqNode(seq :+ b)
  }
  case class ExprRef(override val name: String, private var expr: Expr) extends Expr(name) {
    private[Grammar] def set(newExpr: Expr) {
      expr = newExpr
    }
  }
  case class SyntacticPredicateFail(predToFail: Expr, e: Expr) extends Expr("!(%s) -> %s".format(predToFail, e))
  case class Not(e: Expr) extends Expr("!(%s)".format(e))
  case class CharRange(a: String, b: String) extends Expr("[%s-%s]".format(a, b)) {
    require(a.length == 1)
    require(b.length == 1)
    private val begin = a.charAt(0).toInt
    private val end = b charAt (0).toInt
    require(begin <= end)
    def pred(i:Int): Boolean = { begin <= i && i <= end }
  }
  case class CharPred(override val name: String, pred: Int => Boolean) extends Expr(name)
  case class Leaf(override val name: String, tt: Int) extends Expr(name)
  case class ZeroOrMore(a: Expr) extends Expr("%s*".format(a.name))
  case class OneOrMore(a: Expr) extends Expr("%s+".format(a.name)) {
    def expr = a ~ ZeroOrMore(a)
  }
  case class OptionNode(a: Expr) extends Expr("%s?".format(a.name))
  case class Repeat(a: Expr, separator: Expr) extends Expr("rep(%s, %s)".format(a.name, separator.name)) {
    def expr = OptionNode(a ~ ZeroOrMore(separator ~ a))
  }


}
