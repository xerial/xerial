//--------------------------------------
//
// EqTest.scala
// Since: 2012/08/01 3:52 PM
//
//--------------------------------------

package xerial.core.lens

import xerial.core.XerialSpec

object EqTest {
  case class A(id:Int, name:String, flag:Long) extends Eq
  case class B(id:Int, name:String, flag:Long) extends FastEq
  class MyClass(val id:Int, val name:String) extends Eq
  case class MyClass2(id:Int, name:String) extends FastEq
}



/**
 * @author leo
 */
class EqTest extends XerialSpec {

  import EqTest._

  "Eq trait" should {
    "define equality" in {
      val l = new MyClass(1, "leo")
      l should be (new MyClass(1, "leo"))
      l should not be (new MyClass(3, "leo"))
      l should not be (new MyClass(1, "yui"))
      l.hashCode should be (new MyClass(1, "leo").hashCode)
    }

    "should generate Java code to compare objects" in {
      val c = new MyClass2(1, "leo")

      c should be (new MyClass2(1, "leo"))
      c should not be (new MyClass2(2, "leo"))
      c should not be (new MyClass2(1, "yui"))
    }

    "generate fast comparison code" in {
      import xerial.core.util.StopWatch._


      val c1 = new A(1, "leo", 1L)
      val c2 = new A(1, "leo", 2L)
      val d1 = new B(1, "leo", 1L)
      val d2 = new B(1, "leo", 2L)
      d1.equals(d2)

      time("cmp", repeat=1000) {
        block("reflection", repeat=100) {
          c1.equals(c2)
        }
        block("javassist", repeat=100) {
          d1.equals(d2)
        }
      }

    }

  }
}