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


  def Colon = token(":")
  def Comma = token(",")
  def Hyphen = token("-")

  def comment    = rule { "#" ~ untilEOF }
  def qname      = rule { qnameFirst ~ qnameChar* }
  def qnameFirst = rule { alphabet | "@" | "#" | "." | "_" }
  def qnameChar  = rule { alphabet | digit | "." | "_" }
  def alphabet   = rule { "A" - "Z" | "a" - "z" }
  def indent     = rule { (" " | "\t")+ }
  def Str        = rule { "\"" ~ repeat( "\\" !=>  not("\"") | escapeSequence  ) ~ "\"" }
  def escapeSequence = rule { "\\" ~ ("\"" | "\\" | "/" | "b" | "f" | "n" | "r" | "t" | "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit) }
  def digit      = rule { "0" - "9" }
  def hexDigit   = rule { digit | "A" - "F" | "a" - "f" }

  def silk      = rule { option(indent) ~ (node | context | preamble | comment | dataLine ) }
  def preamble  = rule { "%" ~ qname ~ option(params) }
  def params    = rule { ("(" ~ repeat(param, Comma) ~ ")") | ("-" ~ repeat(param, Comma)) }
  def param     = rule { paramName ~ option(Colon ~ paramValue) }
  def paramName = rule { qname | Str }
  def tuple     = rule { "(" ~ paramValue ~ ")" }
  def paramValue : Expr
                = rule { Str | qname | tuple }

  def node      = rule { "-" ~ nodeBody }
  def nodeBody  = rule {  option(paramName) ~ option(params) ~ option(Colon ~ nodeValue) }
  def nodeValue = rule { untilEOF }
  def dataLine  = rule { untilEOF }
  def context   = rule { "=" ~ nodeBody }
}

/**
 * @author leo
 */
class GrammarTest extends XerialSpec {

  "Grammar" should {
    "be used for defining parsing rules" in {
      val s = SimpleGrammar.silk
      debug(s)
    }

    "be used for defining lexical patterns" in {
      val strRule = SimpleGrammar.Str
      debug(strRule)
    }
  }

}