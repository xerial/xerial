//--------------------------------------
//
// PrioritySearchTree.scala
// Since: 2012/07/30 12:01 PM
//
//--------------------------------------

package xerial.core.collection



/**
 * @author leo
 */
abstract class RedBlackTree[A] {

  protected def op:Interval[A, _]

  private def mkTree(isBlack:Boolean, e:A, l:Tree, r:Tree) : Tree = {
    if(isBlack)
      BlackTree(e, l, r)
    else
      RedTree(e, l, r)
  }

  abstract class Tree {
    def isEmpty : Boolean
    def isBlack : Boolean = true
    def elem : A
    def insert(e:A) : Tree

  }
  object Empty extends Tree {
    def isEmpty = true
    def elem  = null.asInstanceOf[A]
    def insert(e:A) : Tree = RedTree(e, Empty, Empty)
  }

  abstract class NonEmpty extends Tree {
    def left : Tree
    def right: Tree
    def isEmpty = false
    def insert(e:A) : Tree = {
      if(op.startIsSmaller(e, elem))
        balanceLeft(isBlack, elem, left.insert(e), right)
      else
        balanceRight(isBlack, elem, left, right.insert(e))
    }
  }
  case class RedTree(elem:A, left:Tree, right:Tree) extends NonEmpty {
    override def isBlack : Boolean = false
  }
  case class BlackTree(elem:A, left:Tree, right:Tree) extends NonEmpty {
  }

  private def balanceLeft(isBlack:Boolean, e:A, left:Tree, right:Tree) : Tree = left match {
    case RedTree(y, RedTree(x, a, b), c) =>
      RedTree(y, BlackTree(x, a, b), BlackTree(e, c, right))
    case RedTree(x, a, RedTree(y, b, c)) =>
      RedTree(y, BlackTree(x, a, b), BlackTree(e, c, right))
    case _ =>
      mkTree(isBlack, e, left, right)
  }

  private def balanceRight(isBlack:Boolean, e:A, left:Tree, right:Tree) :Tree = right match {
    case RedTree(z, RedTree(y, b, c), d) =>
      RedTree(y, BlackTree(e, left, b), BlackTree(z, c, d))
    case RedTree(y, b, RedTree(z, c, d)) =>
      RedTree(y, BlackTree(e, left, b), BlackTree(z, c, d))
    case _ =>
      mkTree(isBlack, e, left, right)
  }

}

object PrioritySearchTree {

}


class PrioritySearchTree[A](root:RedBlackTree[A]#Tree)(implicit iv:Interval[A, _]) extends RedBlackTree[A] {

  def op = iv

  def insert(e:A) : PrioritySearchTree[A] = {
    new PrioritySearchTree(root.insert(e))
  }





}