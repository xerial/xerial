package xerial.core.io.text

import xerial.core.XerialSpec

//--------------------------------------
//
// LineReaderTest.scala
// Since: 2012/07/20 13:18
//
//--------------------------------------

/**
 * @author leo
 */
class LineReaderTest extends XerialSpec {

  "LineReader" should {
    "support LA(1)" in {
      val m = "hello world"
      val r = LineReader(m)
      for(i <- 0 until m.length) {
        r.LA(1) should be (m(i))
        if(i < m.length - 1)
          r.LA(2) should be (m(i+1))
        r.consume
      }
      r.LA(1) should be (LineReader.EOF)
    }
  }
  
}