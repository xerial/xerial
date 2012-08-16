//--------------------------------------
//
// GrammarTest.scala
// Since: 2012/08/14 2:52 PM
//
//--------------------------------------

package xerial.core.io.text.parser

import xerial.core.XerialSpec
import xerial.core.io.text.parser.Grammar.Expr


object SimpleGrammar extends Grammar {

  def comment    = expr { "#" ~ untilEOF }
  def qname      = expr { qnameFirst ~ repeat(qnameChar) }
  def qnameFirst = expr { alphabet | "." | "_" }
  def qnameChar  = expr { qnameFirst | digit }
  // nameChar allows spaces
  def nameChar   = expr { qnameChar | " " | "!" | "#" | "$" | "%" | "&" | "'" | "*" | "+" | "/" | "-" | ";" | "<" | "=" | ">" | "@"  }
  def alphabet   = expr { "A" ~ "Z" | "a" ~ "z" }
  def indent     = expr { oneOrMore(" " | "\t") }
  def string     = expr { "\"" ~ repeat("\\" !->  not("\"") | escapeSequence ) ~ "\"" }
  def escapeSequence =
                   expr { "\\" ~ ("\"" | "\\" | "/" | "b" | "f" | "n" | "r" | "t" | "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit) }
  def digit      = expr { "0" - "9" }
  def hexDigit   = expr { digit | "A" - "F" | "a" - "f" }

  def silkLine   = expr { option(indent) ~ (node | context | preamble | function | comment | dataLine ) }
  def preamble   = expr { "%" ~ qname ~ option(nodeBody) }
  def namedBody  = expr { name ~ nodeTail}
  def nodeBody   = expr { namedBody | nodeTail }
  def nodeTail   = expr { enclosedParams ~ option(value) | openParams | value }
  def enclosedParams =
                   expr { "(" ~ repeat(param, ",") ~ ")" }
  def openParams = expr { "-" ~ repeat(param, ",") }
  def value      = expr { ":" ~ untilEOF }
  def param      = expr { name ~ option(":" ~ paramValue) }
  def name       = expr { qnameFirst ~ repeat(nameChar) | string }
  def tuple      = expr { "(" ~ paramValue ~ ")" }
  def paramValue : Expr
                 = expr { (string | name | tuple) ~ option(paramOpt) }
  def paramOpt   = expr { "[" ~ repeat(paramValue, ",") ~ "]" }

  def node       = expr { "-" ~ nodeBody }
  def context    = expr { "=" ~ nodeBody }
  def function   = expr { "@" ~ qname ~ option(nodeBody) }
  def dataLine   = expr { untilEOF }

  def whiteSpace = expr { " " | "\t" | "\n" | "\r"  }

  ignore(whiteSpace)
}

/**
 * @author leo
 */
class GrammarTest extends XerialSpec {

  import SimpleGrammar._

  "Grammar" should {
    "be used for defining parsing rules" in {
      val s = silkLine
      debug(s)
    }

    "be used for defining lexical patterns" in {
      val strRule = string
      debug(strRule)
    }

    "parse tuple" in {

      parse(tuple, "(A, 10)")

      Predef println "new"
      Console println  "new"
      def l(f: => Unit) {}

      l { "hello world" }
    }

  }

}