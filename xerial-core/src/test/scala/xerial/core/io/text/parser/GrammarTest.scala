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

  object Preamble extends SymbolToken("%")
  object LParen extends SymbolToken("(")
  object RParen extends SymbolToken(")")
  object QName extends TokenType
  object Colon extends SymbolToken(":")
  object Str extends TokenType


  def silk     = rule { preamble }
  def preamble = rule { Preamble ~ QName ~ option(LParen ~ repeat(param, Colon) ~ RParen) }
  def param    = rule { QName ~ option(Colon ~ paramValue) }
  def tuple    = rule { LParen ~ paramValue ~ RParen }
  def paramValue : Expr
               = rule { Str | QName | tuple }
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
  }

}