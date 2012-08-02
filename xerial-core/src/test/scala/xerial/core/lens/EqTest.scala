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
  case class C(id:Int, name:String, flag:Long)
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

    "shoud generate hash code" in {
      val c = new MyClass2(1, "leo")

      c.hashCode should be (new MyClass2(1, "leo").hashCode)
      c.hashCode should not be (new MyClass2(1, "leo0").hashCode)
      c.hashCode should not be (new MyClass2(34, "leo").hashCode)

    }

    "generate fast comparison code" in {
      import xerial.core.util.StopWatch._

      val c1 = new A(1, "leo", 1L)
      val c2 = new A(1, "leo", 2L)
      val d1 = new B(1, "leo", 1L)
      val d2 = new B(1, "leo", 2L)
      d1.equals(d2)
      val e1 = new C(1, "leo", 1L)
      val e2 = new C(1, "leo", 2L)

      val r = 10000
      time("cmp", repeat=10) {
        block("reflection", repeat=r) {
          c1.equals(c2)
        }
        block("javassist", repeat=r) {
          d1.equals(d2)
        }
        block("case class", repeat=r) {
          e1.equals(e2)
        }
      }

    }

  }
}