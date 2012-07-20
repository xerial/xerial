package xerial.core.io.text

import java.io.{Reader, InputStream, ByteArrayOutputStream}
import java.util.ArrayDeque
import annotation.tailrec


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
  def toRawString(offset: Int, length: Int) = UTF8String(buffer, offset, length)
  def close {
    source.close
  }
  def slide(offset: Int, len: Int) = System.arraycopy(buffer, offset, buffer, 0, len)
  def newBuilder(initialBufferSize: Int) = new ByteArrayBuilder(initialBufferSize)
}

class CharBuffer(source: Reader, private[text] val buffer: Array[Char]) extends TextBuffer {
  def this(source: Reader, bufferSize: Int) = this(source, new Array[Char](bufferSize))

  private def toArray(s: CharSequence) = {
    val a = new Array[Char](s.length)
    for (i <- 0 until s.length)
      a(i) = s.charAt(i)
    a
  }
  def this(s: CharSequence) = this(EmptyReader, toArray(s))

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
  def toRawString = new UTF8String(out.toByteArray)
  def size = out.size
}
class CharArrayBuilder(initialSize: Int) extends TextBuilder {
  private val out = new StringBuilder
  def append(buf: TextBuffer, offset: Int, length: Int) =
    out.append(buf.asInstanceOf[CharBuffer].buffer, offset, length)
  def toRawString = new UTF8String(out.toString)
  def size = out.length
}

object LineReader {
  val EOF = -1;

  def apply(in: InputStream) = apply(in, 8 * 1024) // 8kb buffer
  def apply(in: InputStream, bufferSize: Int) = new LineReader(new ByteBuffer(in, bufferSize))
  def apply(in: Reader) = apply(in, 8 * 1024) // 8kb buffer
  def apply(in: Reader, bufferSize: Int) = new LineReader(new CharBuffer(in, bufferSize))
  def apply(buffer: TextBuffer) = new LineReader(buffer)
  def apply(s: CharSequence) = {
    val buf = bufferOf(s)
    new LineReader(buf, buf.length, true)
  }

  private def bufferOf(s: CharSequence): TextBuffer = {
    s match {
      case str: String => new ByteBuffer(str.getBytes)
      case u: UTF8String => new ByteBuffer(u.byte)
      case _ => new CharBuffer(s)
    }
  }

}

private[LineReader] class ReaderState(var cursor: Int) {
  def this(other: ReaderState) = this(other.cursor)
  override def toString = "%d".format(cursor)
}


/**
 * Fast line reader
 *
 * @author leo
 */
class LineReader(buffer: TextBuffer,
                 private var bufferLimit: Int = 0,
                 private var foundEOF: Boolean = false) {

  private val markQueue = new ArrayDeque[ReaderState]
  private var current = new ReaderState(0)

  def close {
    buffer.close
  }

  def reachedEOF = foundEOF && current.cursor >= bufferLimit

  def nextLine: Option[CharSequence] = {
    var currentLine: TextBuilder = null

    def getCurrentLine : Option[CharSequence] = {
      if (currentLine != null && currentLine.size > 0)
        Some(currentLine.toRawString)
      else
        None
    }

    @tailrec
    def loop: Option[CharSequence] = {
      if (current.cursor >= bufferLimit)
        fill

      if (current.cursor >= bufferLimit)
        getCurrentLine
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

          if (currentLine == null) {
            Some(buffer.toRawString(start, len))
          }
          else {
            currentLine.append(buffer, start, len);
            //incrementLineCount();
            getCurrentLine
          }
        }
        else {
          if (currentLine == null)
            currentLine = buffer.newBuilder(16);
          currentLine.append(buffer, start, len);
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
    if (reachedEOF) {
      false
    }
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
      reachedEOF = true

    if (readBytes == -1)
      false
    else {
      bufferLimit += readBytes
      true
    }
  }

  public Range getSelectedRange() {
    if (markQueue.isEmpty())
      throw new NullPointerException("no mark is set");
    return new Range(markQueue.getLast().cursor, current.cursor);
  }

  public CharSequence selectedRawString() {
    Range r = getSelectedRange();
    return buffer.toRawString(r.begin, r.size());
  }

  public CharSequence selectedRawStringWithTrimming() {
    Range r = trim(getSelectedRange());
    return buffer.toRawString(r.begin, r.size());
  }

  private static class Range {
    public final int begin;
    public final int end;

    public Range (int begin, int end) {
      this.begin = begin;
      this.end = end;
    }

    public int size() {
      return end - begin;
    }

    @Override
    public String toString() {
      return String.format("[%d,%d)", begin, end);
    }
  }

  Range trim (Range input) {
    int begin = input.begin;
    int end = input.end;
    for (;
    begin < end;
    begin ++)
    {
      int c = buffer.get(begin);
      if (c != ' ' && c != '\t' && c != '\r' && c != '\n')
        break;
    }
    for (;
    begin < end;
    end --)
    {
      int c = buffer.get(end - 1);
      if (c != ' ' && c != '\t' && c != '\r' && c != '\n')
        break;
    }
    if (begin >= end) {
      begin = end;
    }
    int size = end - begin;
    return new Range(begin, end);
  }

  Range trim () {
    return trim(getSelectedRange());
  }

  public CharSequence selectedRawString(int margin) {
    if (markQueue.isEmpty())
      return null;
    Range selected = getSelectedRange();
    Range trimmed = new Range(selected.begin + margin, selected.end - margin);
    return buffer.toRawString(trimmed.begin, trimmed.size());
  }

  public CharSequence selectedRawStringFromFirstMark() {
    Range selected = new Range(markQueue.peekFirst().cursor, current.cursor);
    return buffer.toRawString(selected.begin, selected.size());
  }

  public int distanceFromFirstMark() {
    return current.cursor - markQueue.peekFirst().cursor;
  }

  public void mark() {
    markQueue.add(new ScannerState(current));
  }

  public void resetMarks() {
    markQueue.clear();
  }

  /**
   * Reset the stream position to the last marker.
   */
  public void rewind() {
    current = markQueue.pollLast();
  }

}