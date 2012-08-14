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


  def LParen = token("(")
  def RParen = token(")")
  def Colon = token(":")
  def Comma = token(",")
  def Hyphen = token("-")

  def comment = rule { "#" ~ untilEOF }
  def qname = rule { qnameFirst ~ qnameChar* }
  def qnameFirst = rule { alphabet | "@" | "#" | "." | "_" }
  def qnameChar = rule { alphabet | digit | "." | "_" }
  def alphabet = rule { "A" - "Z" | "a" - "z" }
  def indent = rule { (" " | "\t")+ }
  def Str = rule { "\"" ~ repeat( "\\" !=>  not("\"") | escapeSequence  ) ~ "\"" }
  def escapeSequence = rule { "\\" ~ ("\"" | "\\" | "/" | "b" | "f" | "n" | "r" | "t" | "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit) }
  def digit = "0" - "9"
  def hexDigit = rule { digit | "A" - "F" | "a" - "f" }

  def silk     = rule { option(indent) ~ (node | preamble | comment | dataLine ) }
  def preamble = rule { "%" ~ qname ~ option(params) }
  def params = rule { (LParen ~ repeat(param, Comma) ~ RParen) | (Hyphen ~ repeat(param, Comma)) }
  def param    = rule { paramName ~ option(Colon ~ paramValue) }
  def paramName = rule { qname | Str }
  def tuple    = rule { LParen ~ paramValue ~ RParen }
  def paramValue : Expr
               = rule { Str | qname | tuple }

  def node = rule { option(indent) ~ Hyphen ~ option(paramName) ~ option(params) ~ option(Colon ~ nodeValue) }
  def nodeValue = rule { untilEOF }
  def dataLine = rule { untilEOF }
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