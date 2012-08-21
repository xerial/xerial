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
// PrioritySearchTree.scala
// Since: 2012/07/30 12:01 PM
//
//--------------------------------------

package xerial.core.collection



import RedBlackTree._
import xerial.core.log.Logging


object PrioritySearchTree {
  /**
   * element holder
   * @tparam A
   */
  abstract class Holder[A] extends Iterable[A] {
    def elem: A
    def +(e: A): Holder[A]
    def iterator: Iterator[A]
  }

  private[PrioritySearchTree] case class Single[A](elem: A) extends Holder[A] {
    def +(e: A) = Multiple(Vector(elem, e))
    override def toString = "[%s]".format(elem)
    def iterator = Iterator.single(elem)
  }

  /**
   * TODO
   * @param elems
   * @tparam A
   */
  private[PrioritySearchTree] case class Multiple[A](elems: Vector[A]) extends Holder[A] {
    require(elems.length > 1, "elems must have more than one element")
    def elem: A = elems(0)
    def +(e: A) = Multiple(elems :+ e)
    override def toString = "[%s]".format(elems.mkString(", "))
    def iterator = elems.iterator
  }

  def empty[A](implicit iv: IntervalOps[A, Int]) = new PrioritySearchTree[A](null, 0)
}


import PrioritySearchTree._

/**
 * Persistent balanced priority search tree implementation. x-values (interval's start points) are maintained in binary search tree, and the y-values (interval's end points) of the node in the path from the root to leaves
 * are sorted in descending order. This property is good for answering 3-sided queries [x1, x2] x [y1, infinity).
 *
 * This priority search tree allows insertion of the same intervals.
 *
 *
 * @param tree
 * @param size
 * @param iv
 * @tparam A
 */
class PrioritySearchTree[A](tree: Tree[A, Holder[A]], override val size: Int)(implicit iv: IntervalOps[A, Int])
  extends RedBlackTree[A, Holder[A]] with Iterable[A] with Logging {
  type self = PrioritySearchTree[A]

  protected def root : Tree[A, Holder[A]] = if(tree == null) Empty else tree

  override def toString = tree.toString

  protected def isSmaller(a:A, b:A) : Boolean = iv.xIsSmaller(a, b)
  protected def updateTree(t:Tree[A, Holder[A]], key:A, value:Holder[A]) : Tree[A, Holder[A]] = mkTree(t.isBlack, iv.yUpperBound(t.key, key), t.value + key, t.left, t.right)

  /**
   * Create a new key so that it becomes the y-upper bound of the children
   * @param c
   * @param l
   * @param r
   * @return
   */
  override protected def newKey(c: A, l: Option[A], r: Option[A]): A = {
    def m(k1: A, k2: Option[A]): A = k2.map(iv.yUpperBound(k1, _)).getOrElse(k1)
    m(m(c, l), r)
  }
  override protected def newValue(key:A, value:Holder[A]) : Holder[A] = Single(key)

  def +(k: A): self = insert(k)
  def insert(k:A) : self = new PrioritySearchTree(root.update(k, null), size+1)


  def height = {
    def height(t:Tree[A, Holder[A]], h:Int) : Int = {
      if(t.isEmpty)
        h
      else
        math.max(height(t.left, h+1), height(t.right, h+1))
    }

    height(root, 0)
  }


  def iterator = root.iterator.flatMap(_._2)

  def get[A1 <: A](k:A1) : Option[A] = {
    root.lookup(k) match {
      case Empty => None
      case t => t.value.find(iv.==(_, k))
    }
  }

  /**
   * Report the intervals in the tree intersecting with the given range
   * @param range
   * @return
   */
  def queryIntersectingWith(range:A) : Iterator[A] = {
    def find(t:Tree[A, Holder[A]]) : Iterator[A] = {
      if(t.isEmpty || iv.compareXY(range, t.key) > 0) {
        // This tree contains no answer since yUpperBound (t.key.x) < range.x
        Iterator.empty
      }
      else {
        def elementInThisNode = t.value.filter(iv.intersect(_, range))
        def right = if(iv.compareXY(t.key, range) <= 0) t.right.map(find) else Iterator.empty
        t.left.map(find) ++ elementInThisNode ++ right
      }
    }

    find(root)
  }



}


