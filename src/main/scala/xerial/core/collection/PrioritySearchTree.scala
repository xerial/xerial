//--------------------------------------
//
// PrioritySearchTree.scala
// Since: 2012/07/30 12:01 PM
//
//--------------------------------------

package xerial.core.collection


package impl {

abstract class Tree[+A] {
  def isEmpty: Boolean
  def isBlack: Boolean = true
  def elem: A
  def left: Tree[A]
  def right: Tree[A]
}

object Empty extends Tree[Nothing] {
  def left: Tree[Nothing] = Empty
  def right: Tree[Nothing] = Empty
  def isEmpty = true
  def elem = null.asInstanceOf[Nothing]
  override def toString = "Empty"
}


abstract class NonEmpty[+A] extends Tree[A] {
  def isEmpty = false
}


case class RedTree[+A](elem: A, left: Tree[A], right: Tree[A]) extends NonEmpty[A] {
  override def isBlack: Boolean = false
}
case class BlackTree[+A](elem: A, left: Tree[A], right: Tree[A]) extends NonEmpty[A] {
}

}


/**
 * @author leo
 */
abstract class RedBlackTree[A] {


  import impl._

  protected def isSmaller(a:A, b:A) : Boolean


  private def mkTree[B](isBlack: Boolean, e: B, l: Tree[B], r: Tree[B] ): Tree[B] = {
    if (isBlack)
      BlackTree(e, l, r)
    else
      RedTree(e, l, r)
  }

  protected def insert(e:A, t:Tree[A]) : Tree[A] = {
    t match {
      case Empty => RedTree(e, Empty, Empty)
      case _ =>
        if (isSmaller(e, t.elem))
          balanceLeft(t.isBlack, t.elem, insert(e, t.left), t.right)
        else
          balanceRight(t.isBlack, t.elem, t.left, insert(e, t.right))
    }
  }


  protected def balanceLeft[A1 >: A](isBlack: Boolean, e: A, left: Tree[A1], right: Tree[A1]): Tree[A1] = left match {
    case RedTree (y, RedTree (x, a, b), c) =>
      RedTree (y, BlackTree (x, a, b), BlackTree (e, c, right) )
    case RedTree (x, a, RedTree (y, b, c) ) =>
      RedTree (y, BlackTree (x, a, b), BlackTree (e, c, right) )
    case _ =>
      mkTree (isBlack, e, left, right)
  }

  protected def balanceRight[A1 >: A](isBlack: Boolean, e: A, left: Tree[A1], right: Tree[A1]): Tree[A1] = right match {
    case RedTree (z, RedTree (y, b, c), d) =>
      RedTree (y, BlackTree (e, left, b), BlackTree (z, c, d) )
    case RedTree (y, b, RedTree (z, c, d) ) =>
      RedTree (y, BlackTree (e, left, b), BlackTree (z, c, d) )
    case _ =>
      mkTree (isBlack, e, left, right)
  }

}

object PrioritySearchTree {

  def empty[A](implicit iv:IntervalOps[A, _]) = new PrioritySearchTree[A](impl.Empty)

}

/**
 * Persistent balanced priority search tree
 * @param root
 * @param iv
 * @tparam A
 */
class PrioritySearchTree[A](root: impl.Tree[A])(implicit iv: IntervalOps[A, _]) extends RedBlackTree[A] {
  type pst = PrioritySearchTree[A]
  private var count = 0
  protected def isSmaller(a:A, b:A) = iv.startIsSmaller(a, b)

  def +(e:A) : pst = insert(e)
  def insert(e: A): pst = {
    count += 1
    new PrioritySearchTree[A](insert(e, root))
  }

  def size = count

  override def toString = root.toString

}


