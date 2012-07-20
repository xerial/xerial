package xerial.core.io.text

import xerial.core.XerialSpec

//--------------------------------------
//
// UTF8StringTest.scala
// Since: 2012/07/20 10:31
//
//--------------------------------------

/**
 * @author leo
 */
class UTF8StringTest extends XerialSpec {

  "UTF8String" should {
    
    "generate a raw string" in {
      val u = UTF8String("hello world!")
      debug(u)
      u.length should be (12)
    }

    "be held in Set" in {
      val m = Set(UTF8String("hello"), UTF8String("UTF8"))
      debug(m)
      m.size should be (2)
    }
    
  }
  
}