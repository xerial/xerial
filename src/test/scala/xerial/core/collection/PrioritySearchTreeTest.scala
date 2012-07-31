//--------------------------------------
//
// PrioritySearchTreeTest.scala
// Since: 2012/07/30 12:53 PM
//
//--------------------------------------

package xerial.core.collection

import xerial.core.XerialSpec
import util.Random

/**
 * @author leo
 */
class PrioritySearchTreeTest extends XerialSpec {

  "PrioritySearchTree" should {
    "insert new nodes" in {
      var p = PrioritySearchTree.empty[Interval]
      p += Interval(3, 5)
      p += Interval(4, 9)
      p += Interval(4, 12)
      p += Interval(4, 9)
      p += Interval(10, 15)
      p += Interval(6, 11)
      p += Interval(4, 20)
      p += Interval(20, 25)
      p += Interval(28, 32)
      p += Interval(1, 20)
      debug(p)

      debug(p.mkString(", "))
    }

    "insert many nodes" in {
      val r = new Random(0)
      var p = PrioritySearchTree.empty[Interval]
      val n = 50
      for(i <- 0 until n) {
        val s = r.nextInt(10000)
        p += Interval(s, s+100)
      }
      p.size should be (n)
      debug(p)
    }

  }

}