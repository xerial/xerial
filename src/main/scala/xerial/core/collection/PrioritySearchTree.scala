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
  def iterator: Iterator[A]
  def boundary: A
}


abstract class Holder[+A] {
  def elem: A
  def +[A1 >: A](e: A1): Holder[A1]
  def iterator : Iterator[A]
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
  def isLeaf = false
  def iterator: Iterator[A] = left.iterator ++ right.iterator
}

case class RedTree[+A](elem: A, b:Option[A], left: Tree[A], right: Tree[A]) extends Node[A] {
  def isBlack = false
  def boundary = b.getOrElse(elem)
}
case class BlackTree[+A](elem: A, b:Option[A], left: Tree[A], right: Tree[A]) extends Node[A] {
  def isBlack = true
  def boundary = b.getOrElse(elem)
}


/**
 * In PST, every value is stored in a leaf node of the tree.
 * @tparam A
 */
abstract class Leaf[+A] extends Tree[A] {
  def isLeaf = true
  def isBlack = true
  def left = null
  def right = null
  def iterator: Iterator[A]
  def +[A1 >: A](e: A1): Leaf[A1]
  def boundary = elem
}

/**
 * Ghost place holder of values. The actual values appears some nodes up in the tree
 * @param holder
 * @tparam A
 */
case class Ghost[+A](holder: Holder[A]) extends Leaf[A] {
  override def toString = "*%s".format(holder)
  def elem = holder.elem
  def +[A1 >: A](e: A1) = Ghost(holder + e)
  def iterator: Iterator[A] = holder.iterator
}

/**
 * Real holder of the values. No ancestor holds the values in this container
 * @param holder
 * @tparam A
 */
case class Real[+A](holder: Holder[A]) extends Leaf[A] {
  override def toString = "%s".format(holder)
  def elem = holder.elem
  def +[A1 >: A](e: A1) = Real(holder + e)
  def iterator: Iterator[A] = holder.iterator
}

object Empty extends Leaf[Nothing] {
  def elem = null.asInstanceOf[Nothing]
  override def toString = "Empty"
  def iterator: Iterator[Nothing] = Iterator.empty
  def +[A1 >: Nothing](e: A1) = Real(Single(e))
}


}


object PrioritySearchTree {

  def empty[A, V](implicit iv: IntervalOps[A, V]) = new PrioritySearchTree[A, V](impl.Empty)

}

/**
 * Persistent balanced priority search tree. x-coordinate is maintained as binary search tree, and the y-coordinate is
 * @param root
 * @param iv
 * @tparam A
 */
class PrioritySearchTree[A, V](root: impl.Tree[A])(implicit iv: Point2D[A, V]) extends Iterable[A] with Logging {

  import impl._

  type pst = PrioritySearchTree[A, V]
  private var count = 0

  def +(e: A): pst = insert(e)
  def insert(e: A): pst = {
    count += 1
    new PrioritySearchTree[A, V](insert(e, root))
  }

  override def size = count
  override def toString = root.toString

  def iterator = root.iterator


  private def mkTree[B](isBlack: Boolean, e: B, l: Tree[B], r: Tree[B]): Tree[B] = {
    if (isBlack)
      BlackTree(e, None, l, r)
    else
      RedTree(e, None, l, r)
  }


  private def addToLeaf(a: A, leaf: Leaf[A]): Tree[A] = {
    val b = leaf.elem

    val r = if (iv.xIsSmaller(a, b))
      RedTree(leaf.elem, None, Real(Single(a)), leaf)
    else if (iv.xIsSmaller(b, a))
      RedTree(a, None, leaf, Ghost(Single(a)))
    else // a == b
      leaf + a

    //debug("add %s to leaf %s", a, leaf)
    //debug("result %s", r)
    r
  }

  protected def blacken(t:Tree[A]) : Tree[A] = t match {
    case RedTree(e, b, l, r) => BlackTree(e, b, l, r)
    case _ => t
  }


  protected def insert(e: A, tt: Tree[A]): Tree[A] = {
    debug("insert %s", e)

    def insertTo(t: Tree[A]): Tree[A] = {
      t match {
        case n@Empty => n + e
        case g@Ghost(_) => addToLeaf(e, g)
        case r@Real(_) => addToLeaf(e, r)
        case _ =>
          val te = t.elem
          if (iv.xIsSmaller(e, te))
            balanceLeft(t.isBlack, te, insertTo(t.left), t.right)
          else if(iv.xIsSmaller(te, e))
            balanceRight(t.isBlack, te, t.left, insertTo(t.right))
          else { // e.x == te.x
            if (iv.yIsSmaller(e, te))
              balanceRight(t.isBlack, te, t.left, insertTo(t.right))
            else
              balanceRight(t.isBlack, e, t.left, insertTo(t.right))
          }
      }
    }

    val newTree = blacken(insertTo(tt))
    trace("new tree : %s", newTree)
    newTree
  }

  /**
   *
   * v
   * / \
   * w   c
   * / \
   * a   b
   *
   * |
   * V
   *
   * w
   * / \
   * a   v
   * / \
   * b   c
   *
   */
//  protected def fixupLeft(t: Tree[A]): Tree[A] = {
//    debug("fixup left: %s", t)
//    t match {
//      case RedTree(v, RedTree(w, a, b), c) if iv.yIsSmaller(v, w) => {
//        RedTree(w, a, RedTree(v, b, c))
//      }
//      case _ => t
//    }
//  }
//
//  protected def fixupRight(t: Tree[A]): Tree[A] = {
//    debug("fixup right: %s", t)
//    t match {
//      case RedTree(v, a, RedTree(w, b, c)) if iv.yIsSmaller(v, w) => {
//        RedTree(w, RedTree(v, a, b), c)
//      }
//      case _ => t
//    }
//  }
//
//  protected def balance(t:Tree[A]) : Tree[A] = {
//    (t.elem, t.left, t.right) match {
//      case (z, RedTree(x, a, b), RedTree(y, c, d)) =>
//        RedTree(z, BlackTree(x, a, b), BlackTree(y, c, d))
//      case (z, RedTree(x, a, RedTree(y, b, c)), d) =>
//        RedTree(y, BlackTree(x, a, b), BlackTree(z, c, d))
//      case (z, RedTree(y, RedTree(x, a, b), c), d) =>
//        RedTree(y, BlackTree(x, a, b), BlackTree(z, c, d))
//      case (x, a, RedTree(y, b, RedTree(z, c, d))) =>
//        RedTree(y, BlackTree(z, a, b), BlackTree(x, c, d))
//      case (x, a, RedTree(z, RedTree(y, b, c), d)) =>
//        RedTree(y, BlackTree(z, a, b), BlackTree(x, c, d))
//      case _ => t
//    }
//  }


  private def eq(a: A, b: A): Boolean = a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]

  private def newBoundary(c:A, l:A, r:A): Option[A] = {

    val b = if(iv.yIsSmaller(l, r)) {
      if(iv.yIsSmaller(c, r))
        Some(iv.clone(c, iv.x(c), iv.y(r)))
      else
        None
    }
    else {
      if(iv.yIsSmaller(c, l))
        Some(iv.clone(c, iv.x(c), iv.y(l)))
      else
        None
    }
    debug("newBoundary of %s, %s, %s : %s", c, l, r, b)
    b
  }

  protected def balanceLeft(isBlack: Boolean, e: A, left: Tree[A], right: Tree[A]): Tree[A] = left match {
    case yt@RedTree(y, yb, xt@RedTree(x, xb, a, b), c) =>
      RedTree(y, newBoundary(yt.boundary, xt.boundary, e), BlackTree(x, xb, a, b), BlackTree(e, None, c, right))
    case xt@RedTree(x, xb, a, yt@RedTree(y, yb, b, c)) =>
      RedTree(y, newBoundary(yt.boundary, xt.boundary, e), BlackTree(x, xb, a, b), BlackTree(e, None, c, right))
    case _ =>
      mkTree(isBlack, e, left, right)
  }

  protected def balanceRight(isBlack: Boolean, e: A, left: Tree[A], right: Tree[A]): Tree[A] = {
    debug("balance right: e:%s, l:%s, r:%s", e, left, right)
    right match {
    case zt@RedTree(z, zb, yt@RedTree(y, yb, b, c), d) =>
      RedTree(y, newBoundary(yt.boundary, e, zt.boundary), BlackTree(e, None, left, b), BlackTree(z, zb, c, d))
    case yt@RedTree(y, yb, b, zt@RedTree(z, zb, c, d)) =>
      RedTree(y, newBoundary(yt.boundary, e, zt.boundary), BlackTree(e, None, left, b), BlackTree(z, zb, c, d))
    case _ =>
      mkTree(isBlack, e, left, right)
  }
  }

}


