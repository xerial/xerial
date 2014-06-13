package xerial.core.util

import xerial.core.XerialSpec

//--------------------------------------
//
// CommandLineTokenizerTest.scala
// Since: 2012/07/17 18:40
//
//--------------------------------------

/**
 * @author leo
 */
class CommandLineTokenizerTest extends XerialSpec {

  "CommandLineTokenizer" should {
    "tokenize a single string into args" in {
      val args = CommandLineTokenizer.tokenize( """-c "hello world!" -f 3.432""")

      args.length shouldBe 4
      debug {
        args.mkString(", ")
      }
      args should equal(Array("-c", "hello world!", "-f", "3.432"))
    }

  }


}