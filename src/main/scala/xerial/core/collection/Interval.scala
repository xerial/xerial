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

  def xIsSmaller(a:A, b:A) : Boolean
  def yIsSmaller(a:A, b:A) : Boolean
  def xIsSmallerThanOrEq(a:A, b:A) : Boolean
  def yIsSmallerThanOrEq(a:A, b:A) : Boolean
  def minX(a:A, b:A) : A
  def minY(a:A, b:A) : A

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
  def yIsSmaller(a:A, b:A) : Boolean = y(a) < y(b)
  def xIsSmallerThanOrEq(a:A, b:A) : Boolean = x(a) <= x(b)
  def yIsSmallerThanOrEq(a:A, b:A) : Boolean = y(a) <= y(b)
  def minX(a:A, b:A) = if(x(a) <= x(a)) a else b
  def minY(a:A, b:A) = if(y(a) <= y(b)) a else b

  def precede(a:A, b:A) : Boolean = end(a) < start(b)
  def follow(a:A, b:A) : Boolean = end(b) < start(a)
  def intersect(a:A, b:A) : Boolean =  start(a) <= end(b) && start(b) <  end(a)
  def contain(a:A, b:A) : Boolean = start(a) <= start(b) && end(b) <= end(a)
  def startIsSmaller(a:A, b:A) : Boolean = start(a) < start(b)
  def endIsSmaller(a:A, b:A) :Boolean = end(a) < end(b)

}

trait LongInterval[A] extends IntervalOps[A, Long] {
  def xIsSmaller(a:A, b:A) : Boolean = x(a) < x(b)
  def yIsSmaller(a:A, b:A) : Boolean = y(a) < y(b)
  def xIsSmallerThanOrEq(a:A, b:A) : Boolean = x(a) <= x(b)
  def yIsSmallerThanOrEq(a:A, b:A) : Boolean = y(a) <= y(b)
  def minX(a:A, b:A) = if(x(a) <= x(a)) a else b
  def minY(a:A, b:A) = if(y(a) <= y(b)) a else b

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
  override def toString = "[%d,%d)".format(start, end)
}

object Interval {

  implicit object IntIntervalOps extends IntInterval[Interval] {
    def start(a:Interval) = a.start
    def end(a:Interval) = a.end
  }

  def apply(s:Int, e:Int) = new Interval(s, e)
  def point(s:Int) = new Interval(s, s)

}
