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



  def comment    = rule { "#" ~ untilEOF }
  def qname      = rule { qnameFirst ~ qnameChar* }
  def qnameFirst = rule { alphabet | "." | "_" }
  def qnameChar  = rule { qnameFirst | digit }
  // nameChar allows spaces
  def nameChar   = rule { qnameChar | " " | "!" | "#" | "$" | "%" | "&" | "'" | "*" | "+" | "/" | "-" | ";" | "<" | "=" | ">" | "@"  }
  def alphabet   = rule { "A" - "Z" | "a" - "z" }
  def indent     = rule { (" " | "\t")+ }
  def string     = rule { "\"" ~ repeat( "\\" !->  not("\"") | escapeSequence  ) ~ "\"" }
  def escapeSequence =
                   rule { "\\" ~ ("\"" | "\\" | "/" | "b" | "f" | "n" | "r" | "t" | "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit) }
  def digit      = rule { "0" - "9" }
  def hexDigit   = rule { digit | "A" - "F" | "a" - "f" }

  def silkLine   = rule { option(indent) ~ (node | context | preamble | function | comment | dataLine ) }
  def preamble   = rule { "%" ~ qname ~ option(nodeBody) }
  def nodeBody   = rule { option(name) ~ option(enclosedParams ~ option(value) | openParams | value) }
  def enclosedParams =
                  rule { "(" ~ repeat(param, ",") ~ ")" }
  def openParams =
                  rule { "-" ~ repeat(param, ",") }
  def value      = rule { ":" ~ untilEOF }
  def param      = rule { name ~ option(":" ~ paramValue) }
  def name       = rule { qnameFirst ~ repeat(nameChar) | string }
  def tuple      = rule { "(" ~ paramValue ~ ")" }
  def paramValue : Expr
                 = rule { (string | name | tuple) ~ option(paramOpt) }
  def paramOpt   = rule { "[" ~ repeat(paramValue, ",") ~ "]" }

  def node       = rule { "-" ~ nodeBody }
  def context    = rule { "=" ~ nodeBody }
  def function   = rule { "@" ~ qname ~ option(nodeBody) }
  def dataLine   = rule { untilEOF }

  def whiteSpace = rule { " " | "\t" | "\n" | "\r"  }

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

    }
  }

}