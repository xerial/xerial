//--------------------------------------
//
// Interval.scala
// Since: 2012/07/30 11:21 AM
//
//--------------------------------------

package xerial.core.collection


trait Point2D[A, @specialized(Int, Long) V] {
  def x(a:A):V
  def y(a:A):V

  def yUpperBound(a:A, b:A) : A

  def xIsSmaller(a:A, b:A) : Boolean
  def xEquals(a:A, b:A) : Boolean
  def yIsSmaller(a:A, b:A) : Boolean
  def xIsSmallerThanOrEq(a:A, b:A) : Boolean
  def yIsSmallerThanOrEq(a:A, b:A) : Boolean
  def minX(a:A, b:A) : A
  def minY(a:A, b:A) : A
  def maxX(a:A, b:A) : A
  def maxY(a:A, b:A) : A

}


/**
 * Type class representing intervals
 *
 * @author leo
 */
trait IntervalOps[A, @specialized(Int, Long) V] extends Point2D[A, V] {
  def x(a:A) = start(a)
  def y(a:A) = end(a)

  def start(a:A):V
  def end(a:A):V

  def precede(a:A, b:A) : Boolean
  def follow(a:A, b:A) : Boolean
  def intersect(a:A, b:A) : Boolean
  def contain(a:A, b:A) : Boolean
  def startIsSmaller(a:A, b:A) : Boolean
  def endIsSmaller(a:A, b:A) :Boolean
}


trait IntInterval[A] extends IntervalOps[A, Int]{

  def xIsSmaller(a:A, b:A) : Boolean = x(a) < x(b)
  def xEquals(a:A, b:A) : Boolean = x(a) == x(b)
  def yIsSmaller(a:A, b:A) : Boolean = y(a) < y(b)
  def xIsSmallerThanOrEq(a:A, b:A) : Boolean = x(a) <= x(b)
  def yIsSmallerThanOrEq(a:A, b:A) : Boolean = y(a) <= y(b)
  def minX(a:A, b:A) = if(x(a) <= x(b)) a else b
  def minY(a:A, b:A) = if(y(a) <= y(b)) a else b
  def maxX(a:A, b:A) = if(x(a) <= x(b)) a else b
  def maxY(a:A, b:A) = if(y(a) <= y(b)) a else b

  def precede(a:A, b:A) : Boolean = end(a) < start(b)
  def follow(a:A, b:A) : Boolean = end(b) < start(a)
  def intersect(a:A, b:A) : Boolean =  start(a) <= end(b) && start(b) <  end(a)
  def contain(a:A, b:A) : Boolean = start(a) <= start(b) && end(b) <= end(a)
  def startIsSmaller(a:A, b:A) : Boolean = start(a) < start(b)
  def endIsSmaller(a:A, b:A) :Boolean = end(a) < end(b)

}

trait LongInterval[A] extends IntervalOps[A, Long] {
  def xIsSmaller(a:A, b:A) : Boolean = x(a) < x(b)
  def xEquals(a:A, b:A) : Boolean = x(a) == y(a)
  def yIsSmaller(a:A, b:A) : Boolean = y(a) < y(b)
  def xIsSmallerThanOrEq(a:A, b:A) : Boolean = x(a) <= x(b)
  def yIsSmallerThanOrEq(a:A, b:A) : Boolean = y(a) <= y(b)
  def minX(a:A, b:A) = if(x(a) <= x(b)) a else b
  def minY(a:A, b:A) = if(y(a) <= y(b)) a else b
  def maxX(a:A, b:A) = if(x(a) <= x(b)) a else b
  def maxY(a:A, b:A) = if(y(a) <= y(b)) a else b

  def precede(a:A, b:A) : Boolean = end(a) < start(b)
  def follow(a:A, b:A) : Boolean = end(b) < start(a)
  def intersect(a:A, b:A) : Boolean =  start(a) <= end(b) && start(b) <  end(a)
  def contain(a:A, b:A) : Boolean = start(a) <= start(b) && end(b) <= end(a)
  def startIsSmaller(a:A, b:A) : Boolean = start(a) < start(b)
  def endIsSmaller(a:A, b:A) : Boolean = end(a) < end(b)
}


/**
 * Concrete Interval class
 */

class Interval(val start:Int, val end:Int) {
  def size = end - start
  override def toString = "%d:%d".format(start, end)
}
class LInterval(val start:Long, val end:Long) {
  def size = end - start
  override def toString = "%d:%d".format(start, end)
}

object Interval {

  implicit object IntIntervalOps extends IntInterval[Interval] {
    def start(a:Interval) = a.start
    def end(a:Interval) = a.end
     def yUpperBound(a:Interval, b:Interval) : Interval = new Interval(x(a), math.max(y(a), y(b)))
  }

  def apply(s:Int, e:Int) = new Interval(s, e)
  def point(s:Int) = new Interval(s, s)

}


object LInterval {

  implicit object LongIntervalOps extends LongInterval[LInterval] {
    def start(a:LInterval) = a.start
    def end(a:LInterval) = a.end
     def yUpperBound(a:LInterval, b:LInterval) : LInterval = new LInterval(x(a), math.max(y(a), y(b)))
  }

  def apply(s:Long, e:Long) = new LInterval(s, e)
  def point(s:Long) = new LInterval(s, s)

}