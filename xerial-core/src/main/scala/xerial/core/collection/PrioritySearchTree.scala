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

  case class Single[A](elem: A) extends Holder[A] {
    def +(e: A) = Multiple(List(elem, e))
    override def toString = "[%s]".format(elem)
    def iterator = Iterator.single(elem)
  }

  /**
   * TODO
   * @param elems
   * @tparam A
   */
  case class Multiple[A](elems: List[A]) extends Holder[A] {
    require(elems.length > 1, "elems must have more than one element")
    def elem: A = elems(0)
    def +(e: A) = Multiple(e :: elems)
    override def toString = "[%s]".format(elems.mkString(", "))
    def iterator = elems.iterator
  }

  def empty[A](implicit iv: IntervalOps[A, Int]) = new PrioritySearchTree[A](null, 0)
}


import PrioritySearchTree._

/**
 * Persistent balanced priority search tree implementation. x-values are maintained in binary search tree, and the y-values of the node in the path from the root to leaves
 * are sorted in descending order. This property is good for answering 3-sided queries [x1, x2] x [y1, infinity).
 * @param t
 * @param size
 * @param iv
 * @tparam A
 */
class PrioritySearchTree[A](t: Tree[A, Holder[A]], size: Int)(implicit iv: Point2D[A, Int])
  extends RedBlackTree[A, Holder[A]] with Iterable[A] {
  type self = PrioritySearchTree[A]

  protected def root : Tree[A, Holder[A]] = if(t == null) Empty else t

  override def toString = t.toString

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

  def iterator = root.iterator.flatMap(_._2)

  def get[A1 <: A](k:A1) : Option[A] = {
    root.lookup(k) match {
      case Empty => None
      case t => t.value.find(iv.==(_, k))
    }
  }

//  def overlapping(start:Int, end:Int) : Iterator[A] = {
//
//  }
//


}


