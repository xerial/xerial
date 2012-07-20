package xerial.core.io.text

import java.nio.charset.Charset
import collection.IndexedSeqOptimized
import collection.mutable.Builder

//--------------------------------------
//
// UTF8String.scala
// Since: 2012/07/20 9:24
//
//--------------------------------------

object UTF8String {
  val UTF8: Charset = Charset.forName("UTF-8")

  trait Copier[E] {
    def copy(src: Array[E], offset: Int, len: Int): Array[Byte]
  }
  private class CopierByte extends Copier[Byte] {
    def copy(src: Array[Byte], offset: Int, len: Int) = {
      val c = new Array[Byte](len)
      System.arraycopy(src, offset, c, 0, len)
      c
    }
  }
  private class CopierChar extends Copier[Char] {
    def copy(src: Array[Char], offset: Int, len: Int) = new String(src, offset, len).getBytes(UTF8)
  }

  def apply[E](input: Array[E], offset: Int, len: Int)(implicit b: Copier[E]) = new UTF8String(b.copy(input, offset, len))
  def apply(str:String) = new UTF8String(str)
}


/**
 * Raw byte array representation of a string before translating into java string (UCF)
 * @author leo
 */
class UTF8String(private[UTF8String] val byte: Array[Byte]) extends CharSequence with IndexedSeqOptimized[Byte, UTF8String] with Ordered[UTF8String] {

  private[UTF8String] lazy val javaString: String = new String(byte, UTF8String.UTF8)

  def this(s: String) = this(s.getBytes(UTF8String.UTF8))

  def apply(index: Int): Byte = byte(index)
  def getBytes: Array[Byte] = byte
  def length: Int = byte.length
  def seq = byte

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

  override def canEqual(other: Any): Boolean = other.isInstanceOf[UTF8String]

  override def equals(other: Any): Boolean = {
    if (canEqual(other)) {
      val o: UTF8String = other.asInstanceOf[UTF8String]
      if (this.length != o.length)
        false
      else
        this.byte.zip(o.byte).forall(x => x._1 == x._2)
    }
    else
      false
  }

  protected[this] def newBuilder : Builder[Byte, UTF8String] = new UTF8StringBuilder

  def compare(that: UTF8String) = this.javaString.compare(that.javaString)
}


class UTF8StringBuilder extends Builder[Byte, UTF8String] {
  private val b = Array.newBuilder[Byte]
  def +=(elem: Byte) = { b += elem; this }
  def clear() { b. clear }
  def result() = new UTF8String(b.result)
}
