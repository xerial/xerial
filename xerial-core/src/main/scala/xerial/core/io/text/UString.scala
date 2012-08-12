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

package xerial.core.io.text

import java.nio.charset.Charset
import collection.IndexedSeqOptimized
import collection.mutable.Builder

//--------------------------------------
//
// UStringala
// Since: 2012/07/20 9:24
//
//--------------------------------------

object UString {
  val UTF8: Charset = Charset.forName("UTF-8")

  trait Copier[E] {
    def copy(src: Array[E], offset: Int, len: Int): Array[Byte]
  }
  implicit object ByteCopier extends Copier[Byte] {
    def copy(src: Array[Byte], offset: Int, len: Int) = {
      val c = new Array[Byte](len)
      System.arraycopy(src, offset, c, 0, len)
      c
    }
  }
  implicit object CharCopier extends Copier[Char] {
    def copy(src: Array[Char], offset: Int, len: Int) = new String(src, offset, len).getBytes(UTF8)
  }

  def apply[E](input: Array[E], offset: Int, len: Int)(implicit b: Copier[E]) = new UString(b.copy(input, offset, len))
  def apply(str:String) = new UString(str)
}


/**
 * Raw byte array representation of a string before translating into java string (UCF)
 * @author leo
 */
class UString(private[text] val byte: Array[Byte]) extends CharSequence with IndexedSeqOptimized[Byte, UString] with Ordered[UString] {

  private[UString] lazy val javaString: String = new String(byte, UString.UTF8)

  def this(s: String) = this(s.getBytes(UString.UTF8))

  def apply(index: Int): Byte = byte(index)
  def getBytes: Array[Byte] = byte
  def length: Int = byte.length
  def seq = byte

  // The following methods in CharSequences are defined for compatibility with java methods
  override def toString: String = javaString
  def charAt(index: Int): Char = javaString.charAt(index)
  def subSequence(start: Int, end: Int): CharSequence = javaString.subSequence(start, end)

  /**
   * cached hash code
   */
  private var hash: Int = 0

  override def hashCode: Int = {
    var h: Int = hash
    if (h == 0 && length > 0) {
      for (b <- byte) {
        h = 31 * h + b
      }
      hash = h
    }
    return h
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[UString]

  override def equals(other: Any): Boolean = {
    if (canEqual(other)) {
      val o: UString = other.asInstanceOf[UString]
      if (this.length != o.length)
        false
      else
        this.byte.zip(o.byte).forall(x => x._1 == x._2)
    }
    else
      false
  }

  protected[this] def newBuilder : Builder[Byte, UString] = new UStringBuilder

  def compare(that: UString) = this.javaString.compare(that.javaString)
}


class UStringBuilder extends Builder[Byte, UString] {
  private val b = Array.newBuilder[Byte]
  def +=(elem: Byte) = { b += elem; this }
  def clear() { b. clear }
  def result() = new UString(b.result)
}
