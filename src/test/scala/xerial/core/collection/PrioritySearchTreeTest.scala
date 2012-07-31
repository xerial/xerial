//--------------------------------------
//
// PrioritySearchTreeTest.scala
// Since: 2012/07/30 12:53 PM
//
//--------------------------------------

package xerial.core.collection

import xerial.core.XerialSpec

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
      debug(p)

      debug(p.mkString(", "))
    }

  }

}