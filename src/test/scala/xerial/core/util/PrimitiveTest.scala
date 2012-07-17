package xerial.core.util

import xerial.core.XerialSpec

//--------------------------------------
//
// PrimitiveTest.scalaSince: 2012/07/17 22:30
//
//--------------------------------------

/**
 * @author leo
 */
class PrimitiveTest extends XerialSpec {

  "Primitive" should {
    "have all primitives" in {
      for(each <- Primitive.values) {
        debug(each.name)
      }
    }

    "have name" in {
      Primitive.Int.name should be ("Int")
      Primitive.Float.name should be ("Float")
    }
    
  }
  
}