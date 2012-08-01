package xerial.core.io.text

import xerial.core.XerialSpec

//--------------------------------------
//
// UStringTest.scala Since: 2012/07/20 10:31
//
//--------------------------------------

/**
 * @author leo
 */
class UStringTest extends XerialSpec {

  "UString" should {
    
    "generate a raw string" in {
      val u = UString("hello world!")
      debug(u)
      u.length should be (12)
    }

    "be held in Set" in {
      val m = Set(UString("hello"), UString("UTF8"))
      debug(m)
      m.size should be (2)
    }
    
    "be comparable with texts" in {
      val s = UString("%silk - version:1.0")
      s(0) should be ('%')
      s.startsWith("%silk") should be (true)
    }
    
  }
  
}