//--------------------------------------
//
// PrioritySearchTree.scala
// Since: 2012/07/30 12:01 PM
//
//--------------------------------------

package xerial.core.collection

import xerial.core.log.Logging


object RedBlackTree {

  abstract class Tree[+A] {
    def isBlack: Boolean
    def left: Tree[A]
    def right: Tree[A]
    def iterator: Iterator[A]
    def key: A = null.asInstanceOf[A]
    def holder: Holder[A]
  }

  abstract class Holder[+A] extends Iterable[A] {
    def elem: A
    def +[A1 >: A](e: A1): Holder[A1]
    def iterator: Iterator[A]
  }

  case class Single[+A](elem: A) extends Holder[A] {
    def +[A1 >: A](e: A1) = Multiple(Seq(elem, e))
    override def toString = "[%s]".format(elem)
    def iterator = Iterator.single(elem)
  }

  case class Multiple[+A](elems: Seq[A]) extends Holder[A] {
    require(elems.length > 1, "elems must have more than one element")
    def elem: A = elems(0)
    def +[A1 >: A](e: A1) = Multiple(elems :+ e)
    override def toString = "[%s]".format(elems.mkString(", "))
    def iterator = elems.iterator
  }


  abstract class Node[+A] extends Tree[A] {
    def iterator: Iterator[A] = left.iterator ++ holder.iterator ++ right.iterator
  }

  case class RedTree[B](override val key: B, holder: Holder[B], left: Tree[B], right: Tree[B]) extends Node[B] {
    def isBlack = false
  }

  case class BlackTree[B](override val key: B, holder: Holder[B], left: Tree[B], right: Tree[B]) extends Node[B] {
    def isBlack = true
  }

  object Empty extends Tree[Nothing] {
    def isBlack = true
    def left = null
    def right = null
    override def toString = "Empty"
    def holder: Holder[Nothing] = null.asInstanceOf[Holder[Nothing]]
    def iterator: Iterator[Nothing] = Iterator.empty
  }

  def mkTree[A](isBlack: Boolean, key: A, h: Holder[A], l: Tree[A], r: Tree[A]): Tree[A] = {
    if (isBlack)
      BlackTree(key, h, l, r)
    else
      RedTree(key, h, l, r)
  }

  //def contains(e:A) : Boolean = root.lookup(e)

  def blacken[A](t: Tree[A]): Tree[A] = t match {
    case RedTree(k, e, l, r) => BlackTree(k, e, l, r)
    case _ => t
  }

}

import RedBlackTree._

abstract class RedBlackTree[A, Repr](protected val root:Tree[A], override val size:Int) extends Iterable[A] with Logging {

  protected def isSmaller(a:A, b:A) : Boolean
  protected def update(t:Tree[A], e:A) : Tree[A]
  /**
   * Create a new key from the current element and its left/right children. The default is the current element
   * @param e current element
   * @param l
   * @param r
   * @return
   */
  protected def newKey(e:A, l:A, r:A) : A = e

  protected def wrap(t:Tree[A], newSize:Int) : Repr

  def iterator = root.iterator

  override def toString = root.toString
  def +(e: A): Repr = insert(e)
  def insert(e:A) : Repr = wrap(insert(e, root), size+1)

  protected def insert(e: A, tt: Tree[A]): Tree[A] = {
    def insertTo(t: Tree[A]): Tree[A] = t match {
      case n@Empty => RedTree(e, Single(e), Empty, Empty)
      case _ =>
        val k = t.key
        val h = t.holder
        if (isSmaller(e, k))
          balanceLeft(t.isBlack, k, h, insertTo(t.left), t.right)
        else if (isSmaller(k, e))
          balanceRight(t.isBlack, k, h, t.left, insertTo(t.right))
        else
          update(t, e) // e.x == k.x
    }

    blacken(insertTo(tt))
  }

  protected def balanceLeft(isBlack: Boolean, z: A, zh: Holder[A], l: Tree[A], r: Tree[A]): Tree[A] = l match {
    case RedTree(y, yh, RedTree(x, xh, a, b), c) =>
      RedTree(newKey(y, x, z), yh, BlackTree(x, xh, a, b), BlackTree(z, zh, c, r))
    case RedTree(x, xh, a, RedTree(y, yh, b, c)) =>
      RedTree(newKey(y, x, z), yh, BlackTree(x, xh, a, b), BlackTree(z, zh, c, r))
    case _ =>
      mkTree(isBlack, newKey(z, l.key, r.key), zh, l, r)
  }

  protected def balanceRight(isBlack: Boolean, x: A, xh: Holder[A], l: Tree[A], r: Tree[A]): Tree[A] = r match {
    case RedTree(z, zh, RedTree(y, yh, b, c), d) =>
      RedTree(newKey(y, x, z), yh, BlackTree(x, xh, l, b), BlackTree(z, zh, c, d))
    case RedTree(y, yh, b, RedTree(z, zh, c, d)) =>
      RedTree(newKey(y, x, z), yh, BlackTree(x, xh, l, b), BlackTree(z, zh, c, d))
    case _ =>
      mkTree(isBlack, newKey(x, l.key, r.key), xh, l, r)
  }

}

object PrioritySearchTree {
  def empty[A](implicit iv: IntervalOps[A, _]) = new PrioritySearchTree[A](RedBlackTree.Empty, 0)
}




/**
 * Persistent balanced priority search tree implementation. x-values are maintained in binary search tree, and the y-value of the node in the path from the root to leaves
 * are sorted in the descending order. This property is good for answering 3-sided queries [x1, x2] x [y1, infinity).
 * @param root
 * @param iv
 * @tparam A
 */
class PrioritySearchTree[A](root: Tree[A], size: Int)(implicit iv: Point2D[A, _])
  extends RedBlackTree[A, PrioritySearchTree[A]](root, size) {

  protected def isSmaller(a:A, b:A) : Boolean = iv.xIsSmaller(a, b)
  protected def update(t:Tree[A], e:A) : Tree[A] = mkTree(t.isBlack, iv.yUpperBound(t.key, e), t.holder + e, t.left, t.right)
  override protected def newKey(c: A, l: A, r: A): A = {
    def m(k1: A, k2: A): A = Option(k2).map(iv.yUpperBound(k1, _)).getOrElse(k1)
    m(m(c, l), r)
  }

  protected def wrap(t:Tree[A], newSize:Int) : PrioritySearchTree[A] = new PrioritySearchTree(t, newSize)


}


