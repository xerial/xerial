package xerial.core.util

import xerial.core.XerialSpec

//--------------------------------------
//
// JavaProcessTest.scala
// Since: 2012/08/02 10:13
//
//--------------------------------------

/**
 * @author leo
 */
class JavaProcessTest extends XerialSpec {

  "JavaProcess" should {
    "report all running java processes" in {
      val l = JavaProcess.list
      debug(l.mkString(", "))
    }
  }


}