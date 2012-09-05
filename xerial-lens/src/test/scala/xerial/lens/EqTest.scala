//--------------------------------------
//
// EqTest.scala
// Since: 2012/08/01 3:52 PM
//
//--------------------------------------

package xerial.lens

import xerial.core.XerialSpec
import util.Random
import org.scalatest.Tag

object EqTest {
  case class A(id: Int, name: String, flag: Long) extends EqByReflection
  case class B(id: Int, name: String, flag: Long) extends Eq
  case class C(id: Int, name: String, flag: Long)
  class MyClass(val id: Int, val name: String) extends EqByReflection
  case class MyClass2(id: Int, name: String) extends Eq

  case class MyClass3(id:Int, name:String) extends Eq

  case class Nested(a:A) extends Eq
}


/**
 * @author leo
 */
class EqTest extends XerialSpec {

  import EqTest._

  "Eq trait" should {
    "define equality" in {
      val l = new MyClass(1, "leo")
      l should be(new MyClass(1, "leo"))
      l should not be (new MyClass(3, "leo"))
      l should not be (new MyClass(1, "yui"))
      l.hashCode should be(new MyClass(1, "leo").hashCode)
    }

    "generate Java code to compare objects" in {
      val c = new MyClass2(1, "leo")

      c should be(new MyClass2(1, "leo"))
      c should not be (new MyClass2(2, "leo"))
      c should not be (new MyClass2(1, "yui"))
    }

    "generate hash code" in {
      val c = new MyClass2(1, "leo")

      c.hashCode should be(new MyClass2(1, "leo").hashCode)
      c.hashCode should not be (new MyClass2(1, "leo0").hashCode)
      c.hashCode should not be (new MyClass2(34, "leo").hashCode)

    }

    "generate fast comparison code" in {
      import xerial.core.util.StopWatch._
      val c1 = new A(1, "leo", 1L)
      val c2 = new A(1, "leo", 2L)
      val d1 = new B(1, "leo", 1L)
      val d2 = new B(1, "leo", 2L)
      d1.equals(d2) // This generates javassist code
      val e1 = new C(1, "leo", 1L)
      val e2 = new C(1, "leo", 2L)

      val r = 10000
      time("cmp", repeat = 10) {
        block("reflection", repeat = r) {
          c1.equals(c2)
        }
        block("javassist", repeat = r) {
          d1.equals(d2)
        }
        block("case class", repeat = r) {
          e1.equals(e2)
        }
      }
    }

    "be thread-safe" taggedAs("threadsafe") in {
      val r = new Random()
      val nameList = IndexedSeq("A", "B", "C")
      val l = (1 until 2) map {
        i =>
          new MyClass3(r.nextInt(100), nameList(r.nextInt(nameList.length)))
      }
      val g = l.par.groupBy(m => m)
 
    }

    "allow nesting" in {
      val n = Nested(A(1, "leo", 10L))
      val n2 = Nested(A(1, "leo", 10L))
      n.hashCode should be(n2.hashCode)
      n should be (n2)
    }

  }
}