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
    import xerial.core.util.StopWatch._

    var overlapped: Array[Interval] = null
    var overlapped_ans: Array[Interval] = null

    debug("overlap query: %s", q)
    time("overlap query", repeat=3) {
      block("pst query") {
        overlapped = p.queryIntersectingWith(q).toArray
      }
       block("O(n) search") {
        overlapped_ans = p.filter(_.intersectWith(q)).toArray
      }
    }

    trace("overlapped = %s", overlapped.mkString(", "))
    trace("answer     = %s", overlapped_ans.mkString(", "))


    overlapped should be (overlapped_ans)
  }

  def treeStat(p:PrioritySearchTree[_]) {
    debug("height:%d n:%d, log2(n):%.2f", p.height, p.size, math.log10(p.size) / math.log10(2))
  }

  "PrioritySearchTree" should {
    "insert new nodes" in {
      var p = PrioritySearchTree.empty[Interval]
      p += Interval(3, 5)
      p += Interval(4, 9)
      p += Interval(4, 12)
      p += Interval(4, 9)
      p += Interval(10, 15)
      val p1 = p
      p += Interval(6, 11)
      p += Interval(4, 20)
      p += Interval(20, 25)
      p += Interval(28, 32)
      p += Interval(1, 20)
      debug(p)
      treeStat(p)
      debug(p.mkString(", "))
      debug("p1:%s", p1)

      p.get(Interval(4, 20)) should be ('defined)
      p.get(Interval(4, 8)) should be ('empty)

      val rng = p.range(Some(5), Some(23))
      debug("range:%s", rng.mkString(", "))

      overlapQuery(p, Interval(6, 10))
      overlapQuery(p, Interval(13, 18))


      p.get(GInterval("chr1", 3, 5)) should be ('defined)

      p += GInterval("chr1", 8, 19)
      debug(p)
      debug(p.mkString(", "))

    }



    "insert many nodes" in {
      val r = new Random(0)
      val b = PrioritySearchTree.newBuilder[Interval]
      val n = 100000
      for(i <- 0 until n) {
        val s = r.nextInt(1000000)
        b += Interval(s, s+(100+r.nextInt(1000)))
      }
      val p = b.result
      p.size should be (n)
      treeStat(p)
      overlapQuery(p, Interval(1000, 1100))

    }

  }

}