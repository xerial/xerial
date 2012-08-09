package xerial.core.io.text

import java.io.{Reader, InputStream, ByteArrayOutputStream}
import java.util.ArrayDeque
import annotation.tailrec
import xerial.core.log.Logging


//--------------------------------------
//
// LineReader.scala
// Since: 2012/07/20 10:48
//
//--------------------------------------

trait TextBuffer {
  def length: Int
  def apply(index: Int): Int
  def feed(offset: Int, length: Int): Int
  def toRawString(offset: Int, length: Int): CharSequence
  def close: Unit

  /**
   * Slide the buffer contents [offset, offset+len) to [0,.. len)
   *
   * @param offset
   * @param len
   */
  def slide(offset: Int, len: Int)

  def newBuilder(initialBufferSize: Int): TextBuilder
}

object EmptyInputStream extends InputStream {
  def read() = -1
}
object EmptyReader extends Reader {
  def read(cbuf: Array[Char], off: Int, len: Int) = -1
  def close() {}
}


class ByteBuffer(source: InputStream, private[text] val buffer: Array[Byte]) extends TextBuffer {

  def this(source: InputStream, bufferSize: Int) = this(source, new Array[Byte](bufferSize))
  def this(buffer: Array[Byte]) = this(EmptyInputStream, buffer)

  def length = buffer.length
  def apply(index: Int) = buffer(index)
  def feed(offset: Int, length: Int) = source.read(buffer, offset, length)
  def toRawString(offset: Int, length: Int) = UString(buffer, offset, length)
  def close {
    source.close
  }
  def slide(offset: Int, len: Int) = System.arraycopy(buffer, offset, buffer, 0, len)
  def newBuilder(initialBufferSize: Int) = new ByteArrayBuilder(initialBufferSize)
}

object CharBuffer {
  private[CharBuffer] def toArray(s: CharSequence) = {
    val a = new Array[Char](s.length)
    for (i <- 0 until s.length)
      a(i) = s.charAt(i)
    a
  }
}

class CharBuffer(source: Reader, private[text] val buffer: Array[Char]) extends TextBuffer {
  def this(source: Reader, bufferSize: Int) = this(source, new Array[Char](bufferSize))

  def this(s: CharSequence) = this(EmptyReader, CharBuffer.toArray(s))

  def length = buffer.length
  def apply(index: Int) = buffer(index)
  def feed(offset: Int, length: Int) = source.read(buffer, offset, length)
  def toRawString(offset: Int, length: Int) = new String(buffer, offset, length)
  def close {
    source.close
  }
  def slide(offset: Int, len: Int) = System.arraycopy(buffer, offset, buffer, 0, len)
  def newBuilder(initialBufferSize: Int) = new CharArrayBuilder(initialBufferSize)
}


trait TextBuilder {
  def append(buf: TextBuffer, offset: Int, length: Int)
  def toRawString: CharSequence
  def size: Int
}

class ByteArrayBuilder(initialSize: Int) extends TextBuilder {
  private val out = new ByteArrayOutputStream(initialSize)
  def append(buf: TextBuffer, offset: Int, length: Int) =
    out.write(buf.asInstanceOf[ByteBuffer].buffer, offset, length)
  def toRawString = new UString(out.toByteArray)
  def size = out.size
}
class CharArrayBuilder(initialSize: Int) extends TextBuilder {
  private val out = new StringBuilder
  def append(buf: TextBuffer, offset: Int, length: Int) =
    out.appendAll(buf.asInstanceOf[CharBuffer].buffer, offset, length)
  def toRawString = new UString(out.toString)
  def size = out.length
}

object LineReader {
  val EOF = -1;

  def apply(in: InputStream): LineReader = apply(in, 8 * 1024) // 8kb buffer
  def apply(in: InputStream, bufferSize: Int): LineReader = new LineReader(new ByteBuffer(in, bufferSize))
  def apply(in: Reader): LineReader = apply(in, 8 * 1024) // 8kb buffer
  def apply(in: Reader, bufferSize: Int): LineReader = new LineReader(new CharBuffer(in, bufferSize))
  def apply(buffer: TextBuffer): LineReader = new LineReader(buffer)
  def apply(s: CharSequence): LineReader = {
    val buf = bufferOf(s)
    new LineReader(buf, buf.length, true)
  }

  private def bufferOf(s: CharSequence): TextBuffer = {
    s match {
      case str: String => new ByteBuffer(str.getBytes)
      case u: UString => new ByteBuffer(u.byte)
      case _ => new CharBuffer(s)
    }
  }

}

private[text] class ReaderState(var cursor: Int) {
  def this(other: ReaderState) = this(other.cursor)
  override def toString = "%d".format(cursor)
}


/**
 * Fast line reader. When byte stream is passed to this reader, it is much faster
 * than the standard BufferedReader because LineReader does not translate byte arrays into char arrays.
 *
 * @author leo
 */
class LineReader(buffer: TextBuffer,
                 private var bufferLimit: Int = 0,
                 private var foundEOF: Boolean = false) extends Iterable[CharSequence] with Logging {

  private val markQueue = new ArrayDeque[ReaderState]
  private var current = new ReaderState(0)

  def close {
    buffer.close
  }

  def reachedEOF = foundEOF && current.cursor >= bufferLimit

  def nextLine: Option[CharSequence] = {
    var lineBuf: TextBuilder = null

    def currentLine: Option[CharSequence] = {
      if (lineBuf != null && lineBuf.size > 0)
        Some(lineBuf.toRawString)
      else
        None
    }

    @tailrec
    def loop: Option[CharSequence] = {
      if (current.cursor >= bufferLimit)
        fill

      if (current.cursor >= bufferLimit)
        currentLine
      else {
        var eol = false
        var i = current.cursor
        var ch = LineReader.EOF
        while (!eol && i < bufferLimit) {
          ch = buffer(i)
          if (ch == '\n' || ch == '\r') {
            eol = true
          }
          else
            i += 1
        }

        val start = current.cursor
        val len = i - start
        current.cursor = i + 1

        if (eol) {
          // reached EOL
          if (ch == '\r' && LA(1) == '\n')
            current.cursor += 1

          if (lineBuf == null) {
            Some(buffer.toRawString(start, len))
          }
          else {
            lineBuf.append(buffer, start, len);
            //incrementLineCount();
            currentLine
          }
        }
        else {
          if (lineBuf == null)
            lineBuf = buffer.newBuilder(16);
          lineBuf.append(buffer, start, len);
          loop
        }
      }
    }

    loop
  }

  def consume: Int = {
    if (current.cursor >= bufferLimit && !fill) {
      // No more characters to consume. Do nothing
      LineReader.EOF;
    }
    else {
      val ch = buffer(current.cursor)
      current.cursor += 1
      ch
    }
  }

  /**
   * Peek the character at current position + lookahead.
   *
   * @param lookahead
   * @return
   * @throws IOException
   */
  def LA(lookahead: Int): Int = {
    if (current.cursor + lookahead - 1 >= bufferLimit && !fill) {
      // No more character
      LineReader.EOF
    }
    else {
      // current.cursor might be changed at fill(), so we need to lookup the target position again
      buffer(current.cursor + lookahead - 1);
    }
  }

  private def fill: Boolean = {
    if (foundEOF)
      false
    else {
      // Move the [mark ... limit)
      if (!markQueue.isEmpty()) {
        val mark = markQueue.peekFirst()
        val lenToPreserve = bufferLimit - mark.cursor
        if (lenToPreserve < buffer.length) {
          // Move [mark.cursor, limit) to the [0, ..., mark.cursor)
          if (lenToPreserve > 0)
            buffer.slide(mark.cursor, lenToPreserve)
          bufferLimit = lenToPreserve
          current.cursor -= mark.cursor
          val slideLen = mark.cursor
          import collection.JavaConversions._
          for (each <- markQueue)
            each.cursor -= slideLen
        }
        else {
          // The buffer got too big, invalidate the mark
          markQueue.clear()
          bufferLimit = 0
          current.cursor = 0
        }
      }
      else {
        bufferLimit = 0;
        current.cursor = 0;
      }
      // Read the data from the stream, and fill the buffer
      val readLen = buffer.length - bufferLimit
      val readBytes = buffer.feed(current.cursor, readLen)
      if (readBytes < readLen)
        foundEOF = true

      if (readBytes == -1)
        false
      else {
        bufferLimit += readBytes
        true
      }
    }
  }

  def getSelectedRange: Range = {
    if (markQueue.isEmpty())
      sys.error("no mark is set");
    markQueue.getLast.cursor until current.cursor
  }

  def markStart : Int = markQueue.getLast.cursor

  def selected = {
    val r = getSelectedRange
    buffer.toRawString(r.start, r.length)
  }

  def trimSelected = {
    val sr = getSelectedRange
    val r = trim(sr)
    buffer.toRawString(r.start, r.length)
  }

  private def trim(input: Range): Range = {
    var start = input.start
    var end = input.end

    def isWhiteSpace(c: Int): Boolean = (c == ' ' || c == '\t' || c == '\r' || c == '\n')

    @tailrec
    def trimHead(i: Int): Int = {
      if (i < end && isWhiteSpace(buffer(i)))
        trimHead(i + 1)
      else
        i
    }

    start = trimHead(start)

    @tailrec
    def trimTail(i: Int): Int = {
      if (start < i && isWhiteSpace(buffer(i-1)))
        trimTail(i - 1)
      else
        i
    }

    end = trimTail(end)
    if (start >= end)
      start = end

    Range(start, end)
  }

  def selected(margin: Int) = {
    if (markQueue.isEmpty())
      sys.error("no mark is set")
    val r = getSelectedRange
    val trimmed = (r.start + margin) until (r.end - margin)
    buffer.toRawString(trimmed.start, trimmed.length)
  }

  def selectedFromFirstMark = {
    val r = markQueue.peekFirst().cursor until current.cursor
    buffer.toRawString(r.start, r.length)
  }

  def distanceFromFirstMark = current.cursor - markQueue.peekFirst().cursor

  def mark: Unit = markQueue.add(new ReaderState(current))

  def resetMarks: Unit = markQueue.clear()

  /**
   * Reset the stream position to the last marker.
   */
  def rewind: Unit = {
    current = markQueue.pollLast()
  }

  def iterator = new Iterator[CharSequence] {
    var prev : Option[CharSequence] = None

    def hasNext = {
      if(prev.isDefined)
        true
      else {
        prev = nextLine
        prev.isDefined
      }
    }

    def next() = {
      if(hasNext) {
        val line = prev.get
        prev = None
        line
      }
      else
        throw new NoSuchElementException("next")
    }

  }
}