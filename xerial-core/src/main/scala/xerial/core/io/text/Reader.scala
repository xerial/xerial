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

import java.util.ArrayDeque
import xerial.core.collection.CyclicArray

//--------------------------------------
//
// Reader.scala
// Since: 2012/08/12 12:56
//
//--------------------------------------

/**
 * An interface for reading tokens
 * 
 * @author Taro L. Saito
 */
trait Scanner[@specialized(Char, Int) +T] {

  /**
   * Look-ahead the first token
   * @return
   */
  def first : T

  /**
   * Proceeds a cursor by one
   */
  def consume : this.type

  /**
   * Returns true iff the reader reached the end of the stream
   * @return
   */
  def atEnd : Boolean

  /**
   * Close the stream
   */
  def close : Unit

  protected def cursor : Int
  protected def setCursor(c:Int) : Unit
}

trait TextScanner[@specialized(Char, Int) +T] extends Scanner[T] {
  /**
   * Returns the column position in the current line
   */
  def column : Int

  /**
   * Returns the current line number
   */
  def line : Int

}


trait PositionMark[T] { this: Scanner[T] =>

  private val markQueue = new CyclicArray[Int]

  def mark : Unit = {
    markQueue.append(cursor)
  }
  /**
   * Rewind the scanner cursor to the last marked position
   */
  def rewind : Unit = {
    if(markQueue.isEmpty)
      sys.error("no mark to rewind")

    setCursor(markQueue.removeLast)
  }
  def clearMarks : Unit = markQueue.clear


  protected def shiftMarks(offset:Int) : Unit = {
    for(i <- 0 until markQueue.length)
      markQueue.update(i, markQueue(i) + offset)
  }

}