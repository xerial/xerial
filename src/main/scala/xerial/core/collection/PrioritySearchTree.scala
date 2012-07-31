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
  def key : A
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

case class RedTree[+A](key:A, elem: A, left: Tree[A], right: Tree[A]) extends Node[A] {
  def isBlack = false
}
case class BlackTree[+A](key:A, elem: A, left: Tree[A], right: Tree[A]) extends Node[A] {
  def isBlack = true
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
  def key = elem
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

  def empty[A](implicit iv: IntervalOps[A, _]) = new PrioritySearchTree[A](impl.Empty, 0)

}

/**
 * Persistent balanced priority search tree implementation. x-values are maintained in binary search tree, and the y-value of the node in the path from the root to leaves
 * are sorted in the descending order. This property is good for answering 3-sided queries [x1, x2] x [y1, infinity).
 * @param root
 * @param iv
 * @tparam A
 */
class PrioritySearchTree[A](private val root: impl.Tree[A], override val size:Int)(implicit iv: Point2D[A, _]) extends Iterable[A] with Logging {

  import impl._

  type pst = PrioritySearchTree[A]

  def +(e: A): pst = insert(e)
  def insert(e: A): pst = {
    new PrioritySearchTree[A](insert(e, root), size+1)
  }

  override def toString = root.toString

  def iterator = root.iterator


  private def mkTree[B](isBlack: Boolean, key:B, e: B, l: Tree[B], r: Tree[B]): Tree[B] = {
    if (isBlack)
      BlackTree(key, e, l, r)
    else
      RedTree(key, e, l, r)
  }


  private def addToLeaf(a: A, leaf: Leaf[A]): Tree[A] = {
    val b = leaf.elem

    if (iv.xIsSmaller(a, b))
      RedTree(leaf.elem, leaf.elem, Real(Single(a)), leaf)
    else if (iv.xIsSmaller(b, a))
      RedTree(a, a, leaf, Ghost(Single(a)))
    else // a == b
      leaf + a
  }

  protected def blacken(t:Tree[A]) : Tree[A] = t match {
    case RedTree(k, e, l, r) => BlackTree(k, e, l, r)
    case _ => t
  }


  protected def insert(e: A, tt: Tree[A]): Tree[A] = {

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
    //trace("new tree : %s", newTree)
    newTree
  }


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

  private def newKey(c:A, l:A, r:A): A = iv.yUpperBound(iv.yUpperBound(c, l), r)


  protected def balanceLeft(isBlack: Boolean, e: A, left: Tree[A], right: Tree[A]): Tree[A] = left match {
    case yt@RedTree(yb, y, xt@RedTree(xb, x, a, b), c) =>
      RedTree(newKey(yb, xb, e), y, BlackTree(xb, x, a, b), BlackTree(e, e, c, right))
    case xt@RedTree(xb, x, a, yt@RedTree(yb, y, b, c)) =>
      RedTree(newKey(xb, yb, e), y, BlackTree(xb, x, a, b), BlackTree(e, e, c, right))
    case _ =>
      mkTree(isBlack, newKey(e, left.key, right.key), e, left, right)
  }

  protected def balanceRight(isBlack: Boolean, e: A, left: Tree[A], right: Tree[A]): Tree[A] = {
    right match {
    case zt@RedTree(zb, z, yt@RedTree(yb, y, b, c), d) =>
      RedTree(newKey(yb, e, zb), y, BlackTree(e, e, left, b), BlackTree(zb, z, c, d))
    case yt@RedTree(yb, y, b, zt@RedTree(zb, z, c, d)) =>
      RedTree(newKey(yb, e, zb), y, BlackTree(e, e, left, b), BlackTree(zb, z, c, d))
    case _ =>
      mkTree(isBlack, newKey(e, left.key, right.key), e, left, right)
  }
  }

}


