/*
 * Copyright 2012 Taro L. Saito
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package xerial.core.collection

//--------------------------------------
//
// RedBlackTree.scala
// Since: 2012/08/21 2:27 PM
//
//--------------------------------------

import xerial.core.log.Logger

object RedBlackTree {

  /**
   * Represents nodes in RedBlackTrees
   * @tparam A
   * @tparam B
   */
  abstract class Tree[A, B] {
    def isEmpty: Boolean
    def isBlack: Boolean
    def left: Tree[A, B]
    def right: Tree[A, B]
    def iterator: Iterator[(A, B)] = Iterator.empty
    def key: A
    def value: B
    def getKey: Option[A] = None
    def update(k: A, v: B): Tree[A, B]
    def insert(k: A, v: B): Tree[A, B]
    def lookup(e: A): Tree[A, B]
    def map[C](f: Tree[A, B] => C): C = f(this)
    def foreach[C](f: Tree[A, B] => C): Unit
  }

}

import RedBlackTree._

/**
 * Base class for implementing data structures based on Red-Black trees.
 *
 * Balancing operations (balanceLeft, balanceRight) are based on Okasaki's idea (See also Purely functional data structures by C. Okasaki)
 *
 * TODO:
 * deletion (Kahrs 2001)
 * union (for range operation) (building RedBlackTrees from sorted list in linear time. Appel 2011)
 *
 * @tparam A key type
 * @tparam B value type associated to the key
 */
abstract class RedBlackTree[A, B] extends Logger {

  /**
   * Compare keys
   * @param a
   * @param b
   * @return
   */
  protected def isSmaller(a: A, b: A): Boolean

  /**
   * Update a tree with a given key and value. This method is used for
   * adding a new value to node t without changing the tree structure.
   * @param t
   * @param key
   * @param value
   * @return
   */
  protected def updateTree(t: Tree[A, B], key: A, value: B): Tree[A, B]

  /**
   * Create a new key from the current key and its left/right children. This method returns the curent key in default
   * @param key current key
   * @param lkey
   * @param rkey
   * @return
   */
  protected def newKey(key: A, lkey: Option[A], rkey: Option[A]): A = key

  protected def newValue(key: A, value: B): B = value

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
    def left = Empty
    def right = Empty
    def key = null.asInstanceOf[A]
    def update(k: A, v: B) = blacken(insert(k, v))
    def insert(k: A, v: B) = RedTree(k, newValue(k, v), Empty, Empty)
    def lookup(e: A): Tree[A, B] = this
    def foreach[C](f: Tree[A, B] => C): Unit = {}
  }

  abstract class NonEmpty extends Tree[A, B] {
    def isEmpty = false
    override def iterator: Iterator[(A, B)] = left.iterator ++ Iterator.single((key, value)) ++ right.iterator
    override def getKey = Some(key)
    def value: B
    def update(k: A, v: B): Tree[A, B] = blacken(insert(k, v))
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
    def lookup(e: A): Tree[A, B] = {
      if (isSmaller(e, key))
        left.lookup(e)
      else if (isSmaller(key, e))
        right.lookup(e)
      else
        this
    }
    def foreach[C](f: Tree[A, B] => C): Unit = {
      left.foreach(f)
      f(this)
      right.foreach(f)
    }
  }

  case class RedTree(override val key: A, value: B, left: Tree[A, B], right: Tree[A, B]) extends NonEmpty {
    def isBlack = false
  }

  case class BlackTree(override val key: A, value: B, left: Tree[A, B], right: Tree[A, B]) extends NonEmpty {
    def isBlack = true
  }


}