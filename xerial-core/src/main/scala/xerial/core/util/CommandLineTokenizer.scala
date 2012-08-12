package xerial.core.util

import util.parsing.combinator.RegexParsers
import xerial.core.log.Logging


//--------------------------------------
//
// CommandLineTokenizer.scala
// Since: 2012/07/17 18:38
//
//--------------------------------------

/**
 * Tokenize single string representations of command line arguments into Array[String]
 */
object CommandLineTokenizer extends RegexParsers with Logging {

  private def unquote(s: String): String = s.substring(1, s.length() - 1)

  def stringLiteral: Parser[String] =
    ("\"" + """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""" + "\"").r ^^
      { unquote(_) }
  def quotation: Parser[String] =
    ("'" + """([^'\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""" + "'").r ^^
      { unquote(_) }
  def other: Parser[String] = """([^\"'\s]+)""".r
  def token: Parser[String] = stringLiteral | quotation | other
  def tokens: Parser[List[String]] = rep(token)

  def tokenize(line: String): Array[String] = {
    val p = parseAll(tokens, line)
    val r = p match {
      case Success(result, next) => result
      case Error(msg, next) => {
        warn {
          msg
        }
        List.empty
      }
      case Failure(msg, next) => {
        warn {
          msg
        }
        List.empty
      }
    }
    r.toArray
  }
}
