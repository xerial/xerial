//--------------------------------------
//
// PageInputStream.scala
// Since: 2012/08/20 11:44 AM
//
//--------------------------------------

package xerial.core.io

import java.io._
import xerial.core.log.Logger
import annotation.tailrec


object PageInputStream {
  val DefaultPageSize: Int = 8192
}


trait PagedInput[T] extends RichInput[T] with Iterable[Array[T]] with Logger {

  def pageSize: Int

  if (pageSize <= 0)
    throw new IllegalStateException("page size must be > 0")

  def readNextPage: Array[T] = {
    val page = newArray(pageSize)
    val readLen = readFully(page)
    if (readLen <= 0) {
      null
    }
    else if (readLen < pageSize)
      page.slice(0, readLen)
    else
      page
  }

  override def foreach[U](f: (Array[T]) => U) {
    @tailrec def loop: Unit = {
      val page = readNextPage
      if (page != null) {
        f(page)
        loop
      }
    }

    loop
  }

  override def toArray[B >: Array[T] : ClassManifest]: Array[B] = {
    /*
     Overriding this method is necessary since [[scala.core.TraversableOnce.toArray]]
      wrongly set isTraversableAgain = true but page reader cannot be traverse more than once
      */
    iterator.toArray
  }


  def iterator: Iterator[Array[T]] = new PageIterator

  /**
   * Base implementation of page iterator
   */
  private class PageIterator extends Iterator[Array[T]] {
    private var current: Array[T] = null

    def hasNext = {
      if (current != null)
        true
      else if (reachedEOF)
        false
      else {
        current = readNextPage
        current != null
      }
    }

    def next: Array[T] = {
      if (hasNext) {
        val e = current
        current = null
        e
      }
      else
        Iterator.empty.next
    }
  }


}


import PageInputStream._

/**
 * Page-wise input stream reader
 *
 * @author leo
 */
class PageInputStream(in: InputStream, val pageSize: Int) extends RichInputStream(in) with PagedInput[Byte] {
  def this(input: InputStream) = this(input, DefaultPageSize)
  def this(file: File, byteSize: Int = DefaultPageSize) = this(new FileInputStream(file))
}

/**
 * Page-wise text reader
 * @param in
 * @param pageSize
 */
class PageReader(in: Reader, val pageSize: Int) extends RichReader(in) with PagedInput[Char] {
  def this(in: Reader) = this(in, DefaultPageSize)
  def this(file: File, numCharsInPage: Int = DefaultPageSize) = this(new FileReader(file))
}