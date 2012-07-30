//--------------------------------------
//
// Interval.scala
// Since: 2012/07/30 11:21 AM
//
//--------------------------------------

package xerial.core.collection



/**
 * Type class representing intervals
 *
 * @author leo
 */
trait IntervalOps[A, @specialized(Int, Long) V] {

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
  def precede(a:A, b:A) : Boolean = end(a) < start(b)
  def follow(a:A, b:A) : Boolean = end(b) < start(a)
  def intersect(a:A, b:A) : Boolean =  start(a) <= end(b) && start(b) <  end(a)
  def contain(a:A, b:A) : Boolean = start(a) <= start(b) && end(b) <= end(a)
  def startIsSmaller(a:A, b:A) : Boolean = start(a) < start(b)
  def endIsSmaller(a:A, b:A) :Boolean = end(a) < end(b)

}

trait LongInterval[A] extends IntervalOps[A, Long] {
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
