//--------------------------------------
//
// PrioritySearchTree.scala
// Since: 2012/07/30 12:01 PM
//
//--------------------------------------

package xerial.core.collection

import xerial.core.log.Logging


object RedBlackTree {


  abstract class Tree[A, B] {
    def isEmpty : Boolean
    def isBlack: Boolean
    def left: Tree[A, B]
    def right: Tree[A, B]
    def iterator: Iterator[(A, B)] = Iterator.empty
    def key : A
    def value : B
    def getKey : Option[A] = None
    def update(k:A, v:B) : Tree[A, B]
    def insert(k:A, v:B) : Tree[A, B]

    def lookup(e:A) : Tree[A, B]
  }


}

import RedBlackTree._

/**
 * Base class for implementing red-black tree backed data structures
 *
 * @tparam A key type
 * @tparam B value type associated to the key
 */
abstract class RedBlackTree[A, B] extends Logging {

  protected def isSmaller(a:A, b:A) : Boolean

  protected def updateTree(t:Tree[A, B], key:A, value:B) : Tree[A, B]
  /**
   * Create a new key from the current key and its left/right children. This method returns the curent key in default
   * @param key current key
   * @param lkey
   * @param rkey
   * @return
   */
  protected def newKey(key:A, lkey:Option[A], rkey:Option[A]) : A = key
  protected def newValue(key:A, value:B) : B = value

  def mkTree(isBlack: Boolean, key: A, h: B, l: Tree[A, B], r: Tree[A, B]): Tree[A, B] = {
    if (isBlack)
      BlackTree(key, h, l, r)
    else
      RedTree(key, h, l, r)
  }

  def blacken(t: Tree[A, B]): Tree[A, B] = t match {
    case RedTree(k, e, l, r) => BlackTree(k, e, l, r)
    case _ => t
  }

  object Empty extends Tree[A, B] {
    override def toString = "Empty"

    def value = throw new NoSuchElementException("Empty node has no value")
    def isEmpty = true
    def isBlack = true
    def left = null
    def right = null
    def key = throw new NoSuchElementException("No key for Empty node")
    def update(k:A, v:B) = blacken(insert(k, v))
    def insert(k:A, v:B) = RedTree(k, newValue(k, v), Empty, Empty)

    def lookup(e:A) : Tree[A, B] = this
  }

  abstract class NonEmpty extends Tree[A, B] {
    def isEmpty = false
    override def iterator: Iterator[(A, B)] = left.iterator ++ Iterator.single((key, value)) ++ right.iterator
    override def getKey = Some(key)
    def value : B

    def update(k:A, v:B) : Tree[A, B] = blacken(insert(k, v))
    def insert(k: A, v: B): Tree[A, B] = {
      if (isSmaller(k, key))
        balanceLeft(isBlack, key, value, left.insert(k, v), right)
      else if (isSmaller(key, k))
        balanceRight(isBlack, key, value, left, right.insert(k, v))
      else
        updateTree(this, k, v) // k.x == this.key.x
    }

    private def balanceLeft(isBlack: Boolean, z: A, zv: B, l: Tree[A, B], r: Tree[A, B]): Tree[A, B] = l match {
      case RedTree(y, yv, RedTree(x, xv, a, b), c) =>
        RedTree(newKey(y, Some(x), Some(z)), yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, r))
      case RedTree(x, xv, a, RedTree(y, yv, b, c)) =>
        RedTree(newKey(y, Some(x), Some(z)), yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, r))
      case _ =>
        mkTree(isBlack, newKey(z, l.getKey, r.getKey), zv, l, r)
    }

    private def balanceRight(isBlack: Boolean, x: A, xv: B, l: Tree[A, B], r: Tree[A, B]): Tree[A, B] = r match {
      case RedTree(z, zv, RedTree(y, yv, b, c), d) =>
        RedTree(newKey(y, Some(x), Some(z)), yv, BlackTree(x, xv, l, b), BlackTree(z, zv, c, d))
      case RedTree(y, yv, b, RedTree(z, zv, c, d)) =>
        RedTree(newKey(y, Some(x), Some(z)), yv, BlackTree(x, xv, l, b), BlackTree(z, zv, c, d))
      case _ =>
        mkTree(isBlack, newKey(x, l.getKey, r.getKey), xv, l, r)
    }

    def lookup(e:A) : Tree[A, B] = {
      if(isSmaller(e, key))
        left.lookup(e)
      else if(isSmaller(key, e))
        right.lookup(e)
      else
        this
    }

  }

  case class RedTree(override val key: A, value:B, left: Tree[A, B], right: Tree[A, B]) extends NonEmpty {
    def isBlack = false
  }

  case class BlackTree(override val key: A, value: B, left: Tree[A, B], right: Tree[A, B]) extends NonEmpty {
    def isBlack = true
  }


}

object PrioritySearchTree {
  abstract class Holder[A] extends Iterable[A] {
    def elem: A
    def +(e: A): Holder[A]
    def iterator: Iterator[A]
  }

  case class Single[A](elem: A) extends Holder[A] {
    def +(e: A) = Multiple(Seq(elem, e))
    override def toString = "[%s]".format(elem)
    def iterator = Iterator.single(elem)
  }

  case class Multiple[A](elems: Seq[A]) extends Holder[A] {
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
 * Persistent balanced priority search tree implementation. x-values are maintained in binary search tree, and the y-value of the node in the path from the root to leaves
 * are sorted in the descending order. This property is good for answering 3-sided queries [x1, x2] x [y1, infinity).
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

