/*
 * Copyright 2012 Taro L. Saito
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//--------------------------------------
//
// Interval.scala
// Since: 2012/07/30 11:21 AM
//
//--------------------------------------

package xerial.core.collection


/**
 * Type class for two-dimensional points
 *
 * @tparam A
 * @tparam V
 */
trait Point2D[A, @specialized(Int, Long) V] extends Ordering[A] {
  def x(a:A):V
  def y(a:A):V

  def compare(a:A, b:A) : Int = {
    val diff =  compareX(a, b)
    if(diff == 0) compareY(a, b) else diff
  }

  def ==(a:A, b:A) : Boolean = xEquals(a, b) && yEquals(a, b)

  def yUpperBound(a:A, b:A) : A

  def compareX(a:A, b:A) : Int
  def compareY(a:A, b:A) : Int
  def compareXY(a:A, b:A) : Int

  def xIsSmaller(a:A, b:A) : Boolean = compareX(a, b) < 0
  def xEquals(a:A, b:A) : Boolean = compareX(a, b) == 0
  def yIsSmaller(a:A, b:A) : Boolean = compareY(a, b) < 0
  def yEquals(a:A, b:A) : Boolean = compareY(a, b) == 0

  def xIsSmallerThanOrEq(a:A, b:A) : Boolean = compareX(a, b) <= 0
  def yIsSmallerThanOrEq(a:A, b:A) : Boolean = compareY(a, b) <= 0
  def minX(a:A, b:A) : A = if(compareX(a, b) <= 0) a else b
  def minY(a:A, b:A) : A = if(compareY(a, b) <= 0) a else b
  def maxX(a:A, b:A) : A = if(compareX(a, b) >= 0) a else b
  def maxY(a:A, b:A) : A = if(compareY(a, b) >= 0) a else b

}


/**
 * Type class representing intervals
 * @tparam A  actual class
 * @tparam V  interval value type (Usually it is Int or Long)
 * @author leo
 */
trait IntervalType[A, @specialized(Int, Long) V] extends Point2D[A, V] {

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

/**
 * Base implementation
 * @tparam A  actual class
 * @tparam V  interval value type
 * @author leo
 */
trait IntervalTypeBase[A, V <:Ordered[V]] extends IntervalType[A, V] {

  def precede(a:A, b:A) : Boolean = end(a) <= start(b)
  def follow(a:A, b:A) : Boolean = end(b) <= start(a)
  def intersect(a:A, b:A) : Boolean =  start(a) <= end(b) && start(b) <=  end(a)
  def contain(a:A, b:A) : Boolean = start(a) <= start(b) && end(b) <= end(a)
  def startIsSmaller(a:A, b:A) : Boolean = start(a) < start(b)
  def endIsSmaller(a:A, b:A) :Boolean = end(a) < end(b)

  def compareX(a:A, b:A) : Int = x(a).compareTo(x(b))
  def compareY(a:A, b:A) : Int = y(a).compareTo(y(b))
  def compareXY(a:A, b:A) : Int = x(a).compareTo(y(b))

}


/**
 * Optimized implementation of the interval types class for Int
 * @tparam A repr
 */
trait IntInterval[A] extends IntervalType[A, Int]{

  def compareX(a:A, b:A) = x(a) - x(b)
  def compareY(a:A, b:A) = y(a) - y(b)
  def compareXY(a:A, b:A) = (x(a) - y(b))

  def precede(a:A, b:A) : Boolean = end(a) <= start(b)
  def follow(a:A, b:A) : Boolean = end(b) <= start(a)
  def intersect(a:A, b:A) : Boolean =  start(a) <= end(b) && start(b) <=  end(a)
  def contain(a:A, b:A) : Boolean = start(a) <= start(b) && end(b) <= end(a)
  def startIsSmaller(a:A, b:A) : Boolean = start(a) < start(b)
  def endIsSmaller(a:A, b:A) :Boolean = end(a) < end(b)

}

/**
 * Optimized implementation of the interval types class for Long
 * @tparam A  repr
 */
trait LongInterval[A] extends IntervalType[A, Long] {

  def compareX(a:A, b:A) = (x(a) - x(b)).toInt
  def compareY(a:A, b:A) = (y(a) - y(b)).toInt
  def compareXY(a:A, b:A) = (x(a) - y(b)).toInt

  def precede(a:A, b:A) : Boolean = end(a) <= start(b)
  def follow(a:A, b:A) : Boolean = end(b) <= start(a)
  def intersect(a:A, b:A) : Boolean =  start(a) <= end(b) && start(b) <=  end(a)
  def contain(a:A, b:A) : Boolean = start(a) <= start(b) && end(b) <= end(a)
  def startIsSmaller(a:A, b:A) : Boolean = start(a) < start(b)
  def endIsSmaller(a:A, b:A) : Boolean = end(a) < end(b)
}


/**
 * Closed interval [start, end], where start and end are Int values
 */
class Interval(val start:Int, val end:Int)  {
  require(start <= end, "start must be smaller than end: [%d, %d]".format(start, end))

  def size = end - start
  override def toString = "%d:%d".format(start, end)

  def r : Range = Range(start, end)


  import Interval.IntIntervalType._

  def intersectWith(other: Interval) : Boolean = intersect(this, other)
  def contains(other:Interval) : Boolean = contain(this, other)
  def contains(pos:Int) : Boolean = start <= pos && pos <= end

}

/**
 * Closed interval [start, end] where start and end are Long values
 * @param start
 * @param end
 */
class LInterval(val start:Long, val end:Long) {
  require(start <= end, "start must be smaller than end: [%d, %d]".format(start, end))

  def size = end - start
  override def toString = "%d:%d".format(start, end)

  import LInterval.LongIntervalType._

  def intersectWith(other: LInterval) : Boolean = intersect(this, other)
  def contains(other:LInterval) : Boolean = contain(this, other)
  def contains(pos:Long) : Boolean = start <= pos && pos <= end

}

object Interval {

  implicit object IntIntervalType extends IntInterval[Interval] {
    def start(a:Interval) = a.start
    def end(a:Interval) = a.end
    def yUpperBound(a:Interval, b:Interval) : Interval = new Interval(x(a), math.max(y(a), y(b)))
  }

  def apply(s:Int, e:Int) = new Interval(s, e)
  def point(s:Int) = new Interval(s, s)

}


object LInterval {

  implicit object LongIntervalType extends LongInterval[LInterval] {
    def start(a:LInterval) = a.start
    def end(a:LInterval) = a.end
    def yUpperBound(a:LInterval, b:LInterval) : LInterval = new LInterval(x(a), math.max(y(a), y(b)))
  }

  def apply(s:Long, e:Long) = new LInterval(s, e)
  def point(s:Long) = new LInterval(s, s)

}