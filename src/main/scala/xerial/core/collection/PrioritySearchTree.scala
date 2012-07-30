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
  def isLeaf: Boolean
  def isBlack: Boolean
  def elem: A
  def left: Tree[A]
  def right: Tree[A]
  def iterator : Iterator[A]
}


abstract class Node[+A] extends Tree[A] {
  def isLeaf = false
  def iterator :Iterator[A] = left.iterator ++ right.iterator

}


case class RedTree[+A](elem: A, left: Tree[A], right: Tree[A]) extends Node[A] {
  def isBlack = false
}
case class BlackTree[+A](elem: A, left: Tree[A], right: Tree[A]) extends Node[A] {
  def isBlack = true
}

case class EmptyNode[A](left:PlaceHolder[A], right:PlaceHolder[A]) extends Node[A] {
  def isBlack = false
  def elem = null.asInstanceOf[A]
}

abstract class PlaceHolder[+A] extends Tree[A] {
  def isLeaf = true
  def isBlack = true
  def left = null
  def right = null
  def iterator :Iterator[A] = Iterator.single(elem)
}


case class BlankLeaf[A](elem: A) extends PlaceHolder[A] {
  override def toString = "*%s".format(elem)
}
case class Leaf[A](elem: A) extends PlaceHolder[A] {
  override def toString = "%s".format(elem)
}

object Empty extends PlaceHolder[Nothing] {
  def elem = null.asInstanceOf[Nothing]
  override def toString = "Empty"
  override def iterator: Iterator[Nothing] = Iterator.empty
}



}


object PrioritySearchTree {

  def empty[A](implicit iv: IntervalOps[A, _]) = new PrioritySearchTree[A](impl.Empty)

}

/**
 * Persistent balanced priority search tree. x-coordinate is maintained as binary search tree, and the y-coordinate is
 * @param root
 * @param iv
 * @tparam A
 */
class PrioritySearchTree[A](root: impl.Tree[A])(implicit iv: Point2D[A, _]) extends Iterable[A] with Logging {

  import impl._

  type pst = PrioritySearchTree[A]
  private var count = 0

  def +(e: A): pst = insert(e)
  def insert(e: A): pst = {
    count += 1
    new PrioritySearchTree[A](insert(e, root))
  }

  override def size = count
  override def toString = root.toString

  def iterator = root.iterator


  private def mkTree[B](isBlack: Boolean, e: B, l: Tree[B], r: Tree[B]): Tree[B] = {
    if (isBlack)
      BlackTree(e, l, r)
    else
      RedTree(e, l, r)
  }

  private def newPlaceHolder(newElem:A, p:PlaceHolder[A]) : Tree[A] = {
    if (iv.xIsSmallerThanOrEq(newElem, p.elem))
      EmptyNode(Leaf(newElem), p)
    else
      EmptyNode(p, Leaf(newElem))
  }

  protected def insert(e: A, tt: Tree[A]): Tree[A] = {
    debug("insert %s", e)

    def insertTo(t:Tree[A]) : Tree[A] = t match {
      case Empty => Leaf(e)
      case b@BlankLeaf(elem) => newPlaceHolder(e,b)
      case l@Leaf(elem) => newPlaceHolder(e,l)
      case _ =>
        if (iv.xIsSmallerThanOrEq(e, t.elem))
          mkTree(t.isBlack, t.elem, insertTo(t.left), t.right)
        else
          mkTree(t.isBlack, t.elem, t.left, insertTo(t.right))
    }

    val newTree = insertTo(tt)
    trace("new tree : %s", newTree)
    val p = pushDown(e, newTree)
    trace("push down: %s", p)
    p
  }

  private def eq(a:A, b:A) : Boolean = a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]

  private def pushDown(isBlack:Boolean, e:A, v:A, l:Tree[A], r:Tree[A]) : Tree[A] = {
    val (n, ee) = if(iv.yIsSmallerThanOrEq(e, v)) (v, e) else  (e, v)
    if(!eq(e, ee))
      debug("push down %s under %s", ee, n)
    if(iv.xIsSmallerThanOrEq(ee, n))
      mkTree(isBlack, n, pushDown(ee, l), r)
    else
      mkTree(isBlack, n, l, pushDown(ee, r))
  }

  protected def pushDown(e:A, t:Tree[A]) : Tree[A] = t match {
    case Empty => sys.error("cannot reach empty leaf: " + t)
    case EmptyNode(l, r) =>
      if(eq(e, l.elem))
        RedTree(e, BlankLeaf(e), r)
      else if(eq(e, r.elem))
        RedTree(e, l, BlankLeaf(e))
      else
        RedTree(e, l, r)
    case BlankLeaf(l) => t
    case Leaf(l) => t
    case RedTree(v, l, r) => pushDown(false, e, v, l, r)
    case BlackTree(v, l, r) => pushDown(true, e, v, l, r)
  }


  protected def balanceLeft[A1 >: A](isBlack: Boolean, e: A, left: Tree[A1], right: Tree[A1]): Tree[A1] = left match {
    case RedTree(y, RedTree(x, a, b), c) =>
      RedTree(y, BlackTree(x, a, b), BlackTree(e, c, right))
    case RedTree(x, a, RedTree(y, b, c)) =>
      RedTree(y, BlackTree(x, a, b), BlackTree(e, c, right))
    case _ =>
      mkTree(isBlack, e, left, right)
  }

  protected def balanceRight[A1 >: A](isBlack: Boolean, e: A, left: Tree[A1], right: Tree[A1]): Tree[A1] = right match {
    case RedTree(z, RedTree(y, b, c), d) =>
      RedTree(y, BlackTree(e, left, b), BlackTree(z, c, d))
    case RedTree(y, b, RedTree(z, c, d)) =>
      RedTree(y, BlackTree(e, left, b), BlackTree(z, c, d))
    case _ =>
      mkTree(isBlack, e, left, right)
  }

}


