//--------------------------------------
//
// StringScanner.scala
// Since: 2012/08/15 1:29 PM
//
//--------------------------------------

package xerial.core.io.text

/**
 * Simple scanner implementation for String
 *
 * @author leo
 */
class StringScanner(s:String) extends Scanner with PositionMarkImpl {

  private var _cursor = 0

  /**
   * Look-ahead the first character
   * @return
   */
  def first = if(atEnd) EOF else s.charAt(_cursor)
  /**
   * Proceeds a cursor by one
   */
  def consume = {
    _cursor += 1
    this
  }
  /**
   * Returns true iff the reader reached the end of the stream
   * @return
   */
  def atEnd = _cursor == s.length
  /**
   * Close the stream
   */
  def close {}
  protected def cursor = _cursor
  protected def setCursor(c: Int) {
    _cursor = c
  }

}