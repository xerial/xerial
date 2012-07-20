package xerial.core.io.text

import xerial.core.XerialSpec
import java.io.{StringReader, ByteArrayInputStream}

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
    
    "support mark and rewind" in {
      val m = "Hello World"
      val r = LineReader(m)
      r.mark
      for(i <- 0 until 4)
        r.consume

      r.LA(1) should be ('o')
      r.selected.toString should be ("Hell")
      r.rewind
      for(i <- 0 until m.length) {
        r.LA(1) should be (m.charAt(i))
        r.consume
      }
      r.LA(1) should be (LineReader.EOF)
    }

    val sample = "Hello World\nThanks for using xerial-core!!!\nTaro L. Saito";
    
    "support byte arrays" in {
      val b = sample.getBytes("UTF-8")
      val s = LineReader(new ByteArrayInputStream(b), 10) // use short buffer

      for(i <- 0 until sample.length) {
        s.LA(1) should be (sample.charAt(i))
        s.consume
      }
      s.LA(1) should be (-1)
    }

    "support multiple marks" in {
      val s = LineReader(new StringReader(sample))
      s.mark
      for(i <- 0 until 28) {
        if(s.LA(1) == '\n') {
          s.consume
          s.resetMarks
          s.mark
        }
        else {
          s.consume
          if(i % 5 == 0)
            s.mark
        }
      }

      val c = s.selectedFromFirstMark
      c.toString should be ("Thanks for using")
    }

    "read lines" in {
      val s = LineReader(sample)
      val line = s.map(_.toString).toIndexedSeq
      line.length should be (3)
      line(0) should be ("Hello World")
      line(1) should be ("Thanks for using xerial-core!!!")
      line(2) should be ("Taro L. Saito")
    }


  }
  
}