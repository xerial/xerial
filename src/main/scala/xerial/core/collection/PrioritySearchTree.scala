//--------------------------------------
//
// PrioritySearchTree.scala
// Since: 2012/07/30 12:01 PM
//
//--------------------------------------

package xerial.core.collection

import xerial.core.log.Logging


package impl {

abstract class Tree[+A] {
  def isBlack: Boolean
  def left: Tree[A]
  def right: Tree[A]
  def iterator: Iterator[A]
  def key: A = null.asInstanceOf[A]
  def holder: Holder[A]
}


abstract class Holder[+A] {
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

case class RedTree[+A](override val key: A, holder: Holder[A], left: Tree[A], right: Tree[A]) extends Node[A] {
  def isBlack = false
}

case class BlackTree[+A](override val key: A, holder: Holder[A], left: Tree[A], right: Tree[A]) extends Node[A] {
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

}


object PrioritySearchTree {

  def empty[A](implicit iv: IntervalOps[A, _]) = new PrioritySearchTree[A](impl.Empty, 0)

}

/**
 * Persistent balanced priority search tree implementation. x-values are maintained in binary search tree, and the y-value of the node in the path from the root to leaves
 * are sorted in the descending order. This property is good for answering 3-sided queries [x1, x2] x [y1, infinity).
 * @param root
 * @param iv
 * @tparam A
 */
class PrioritySearchTree[A](private val root: impl.Tree[A], override val size: Int)(implicit iv: Point2D[A, _]) extends Iterable[A] with Logging {

  import impl._

  type pst = PrioritySearchTree[A]

  def +(e: A): pst = insert(e)
  def insert(e: A): pst = {
    new PrioritySearchTree[A](insert(e, root), size + 1)
  }

  override def toString = root.toString

  def iterator = root.iterator


  private def mkTree[B](isBlack: Boolean, key: B, h: Holder[B], l: Tree[B], r: Tree[B]): Tree[B] = {
    if (isBlack)
      BlackTree(key, h, l, r)
    else
      RedTree(key, h, l, r)
  }


  protected def blacken(t: Tree[A]): Tree[A] = t match {
    case RedTree(k, e, l, r) => BlackTree(k, e, l, r)
    case _ => t
  }

  protected def insert(e: A, tt: Tree[A]): Tree[A] = {

    def insertTo(t: Tree[A]): Tree[A] = t match {
      case n@Empty => RedTree(e, Single(e), Empty, Empty)
      case _ =>
        val k = t.key
        val h = t.holder
        if (iv.xIsSmaller(e, t.key))
          balanceLeft(t.isBlack, k, h, insertTo(t.left), t.right)
        else if (iv.xIsSmaller(t.key, e))
          balanceRight(t.isBlack, k, h, t.left, insertTo(t.right))
        else {
          // e.x == te.x
          mkTree(t.isBlack, iv.yUpperBound(k, e), h + e, t.left, t.right)
        }
    }


    val newTree = blacken(insertTo(tt))
    //trace("insert %s, new tree:\n%s", e, newTree)
    newTree
  }

  private def newKey(c: A, l: A, r: A): A = {
    def m(k1: A, k2: A): A = Option(k2).map {
      iv.yUpperBound(k1, _)
    } getOrElse k1
    val k = m(m(c, l), r)
    k
  }

  protected def balanceLeft(isBlack: Boolean, k: A, h: Holder[A], left: Tree[A], right: Tree[A]): Tree[A] = left match {
    case yt@RedTree(yb, y, xt@RedTree(xb, x, a, b), c) =>
      RedTree(newKey(yb, xb, k), y, BlackTree(xb, x, a, b), BlackTree(k, h, c, right))
    case xt@RedTree(xb, x, a, yt@RedTree(yb, y, b, c)) =>
      RedTree(newKey(xb, yb, k), y, BlackTree(xb, x, a, b), BlackTree(k, h, c, right))
    case _ =>
      mkTree(isBlack, newKey(k, left.key, right.key), h, left, right)
  }

  protected def balanceRight(isBlack: Boolean, k: A, h: Holder[A], left: Tree[A], right: Tree[A]): Tree[A] = {
    right match {
      case zt@RedTree(zb, z, yt@RedTree(yb, y, b, c), d) =>
        RedTree(newKey(yb, k, zb), y, BlackTree(k, h, left, b), BlackTree(zb, z, c, d))
      case yt@RedTree(yb, y, b, zt@RedTree(zb, z, c, d)) =>
        RedTree(newKey(yb, k, zb), y, BlackTree(k, h, left, b), BlackTree(zb, z, c, d))
      case _ =>
        mkTree(isBlack, newKey(k, left.key, right.key), h, left, right)
    }

  }

}


