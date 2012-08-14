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

//--------------------------------------
//
// Reader.scala
// Since: 2012/08/12 12:56
//
//--------------------------------------

/**
 * An interface for reading text streams
 * 
 * @author Taro L. Saito
 */
trait Scanner[@specialized(Char, Int) +T] {

  /**
   * Look-ahead the first character
   * @return
   */
  def first : T = lookAhead(1)

  /**
   * Look-ahead the k-th character from the current position
   * @param k
   * @return
   */
  def lookAhead(k:Int) : T

  /**
   * Returns a reader containing the remaining input except the fist
   * @return
   */
  def rest : Scanner[T]

  /**
   * Returns true iff the reader reached the end of the stream
   * @return
   */
  def atEnd : Boolean

  /**
   * Returns the column position in the current line
   */
  def column : Int

  /**
   * Returns the current line number
   */
  def line : Int

  /**
   * Close the stream
   */
  def close : Unit
}


trait PositionMark {

  //private val markQueue : Array[Int] = Array.ofDim(2)

  def mark : Unit
  def clearMarks : Unit

  protected def shiftMarks(offset:Int) : Unit = {
    //markQueue
  }

}