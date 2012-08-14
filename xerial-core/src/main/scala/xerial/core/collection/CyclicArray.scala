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

package xerial.core.collection

import collection.mutable.{IndexedSeqOptimized, ArrayOps, ArrayLike}


//--------------------------------------
//
// CyclicArray.scalaa
// Since: 2012/08/12 14:13
//
//--------------------------------------

object CyclicArray {


}

/**
 * A mutable array that supports append and prepend operations in O(1) amortized complexity.
 * 
 * @author Taro L. Saito
 */
class CyclicArray[@specialized A](implicit m:ClassManifest[A]) extends IndexedSeqOptimized[A, CyclicArray[A]] {
  type self = this.type 

  private var queue = m.newArray(8) // queue size must be 2^i
  private var h = 0
  private var t = 0

  def apply(i:Int) = queue(index(i))

  @inline private def index(i:Int) = i & (queue.length - 1) // mode (QUEUE_SIZE - 1)

  def peekFirst : A = queue(h)
  def peekLast : A = queue(index(t-1))

  def addFirst(e:A) : self = {
    h = index(h - 1)
    queue(h) = e
    if(h == t)
      doubleCapacity
    this
  }

  def addLast(e:A) : self = {
    queue(t) = e
    t = index(t + 1) // mod (QUEUE_SIZE)
    if(h == t)
      doubleCapacity
    this
  }

  override def size = index(t - h)
  override def isEmpty = h == t


  private def doubleCapacity {
    assert(h == t)
    val p = h
    val n = queue.length
    val r = n - p // the number of elements to the right of p
    val newCapacity = n << 1
    if(newCapacity < 0)
      sys.error("Too big queue size: %,d".format(newCapacity))
    val newQueue = m.newArray(newCapacity)
    Array.copy(queue, p, newQueue, 0, r)
    Array.copy(queue, 0, newQueue, r, p)
    queue = newQueue
    h = 0
    t = n
  }

  protected[this] def newBuilder = null

  def length = size

  def update(idx: Int, elem: A) { queue(idx) = elem}

  def seq = {
    val b = Seq.newBuilder[A]
    for(i <- 0 until length)
      b += apply(i)
    b.result
  }

}