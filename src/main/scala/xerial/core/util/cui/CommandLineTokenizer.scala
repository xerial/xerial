package xerial.core.util.cui

import util.parsing.combinator.RegexParsers
import xerial.core.util.Logging


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

  protected def unquote(s: String): String = s.substring(1, s.length() - 1)

  protected def stringLiteral: Parser[String] =
    ("\"" + """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""" + "\"").r ^^ {
      unquote(_)
    }

  protected def quotation: Parser[String] =
    ("'" + """([^'\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""" + "'").r ^^ {
      unquote(_)
    }

  protected def other: Parser[String] = """([^\"'\s]+)""".r

  protected def token: Parser[String] = stringLiteral | quotation | other

  protected def tokens: Parser[List[String]] = rep(token)

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
