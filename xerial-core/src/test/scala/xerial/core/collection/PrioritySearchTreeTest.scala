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

  case class GInterval(chr:String, override val start:Int, override val end:Int) extends Interval(start, end) {
    override def toString = "%s-%d:%d".format(chr, start, end)
  }

  def overlapQuery(p:PrioritySearchTree[Interval], q:Interval) {
    debug("overlap query: %s", q)
    val overlapped = p.queryIntersectingWith(q).toArray
    debug("overlapped = %s", overlapped.mkString(", "))
    val overlapped_ans = p.filter(_.intersectWith(q)).toArray
    debug("answer     = %s", overlapped_ans.mkString(", "))
    overlapped should be (overlapped_ans)
  }

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

      p.get(Interval(4, 20)) should be ('defined)
      p.get(Interval(4, 8)) should be ('empty)


      overlapQuery(p, Interval(6, 10))
      overlapQuery(p, Interval(13, 18))

      p.get(GInterval("chr1", 3, 5)) should be ('defined)

      p += GInterval("chr1", 8, 19)
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

      overlapQuery(p, Interval(1000, 1500))

    }

  }

}