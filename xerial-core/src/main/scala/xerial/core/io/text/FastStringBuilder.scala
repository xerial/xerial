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

//--------------------------------------
//
// FastStringBuilder.scala
// Since: 2012/10/04 10:09 AM
//
//--------------------------------------

package xerial.core.io.text




trait StringHolder {
  def +(s:CharSequence) : StringHolder
  def clear: Unit
  def result: CharSequence
  def isEmpty : Boolean
}

import FastStringBuilder._
/**
 * @author leo
 */
class FastStringBuilder(private var holder : StringHolder = StringHolder0) {
  def +=(s:CharSequence) : this.type = {
    holder += s
    this
  }
  def clear : this.type = {
    holder.clear
    holder = StringHolder0
    this
  }
  def isEmpty : Boolean = holder.isEmpty
  def result : CharSequence = holder.result
}


object FastStringBuilder {

  private val emptyString = ""

  def newBuilder : FastStringBuilder = new FastStringBuilder(StringHolder0)


  object StringHolder0 extends StringHolder {
    def +(s: CharSequence) = new StringHolder1(s)
    def clear {}
    def result = emptyString
    def isEmpty : Boolean = true
  }

  class StringHolder1(private var str:CharSequence) extends StringHolder {
    def +(s: CharSequence) = {
      val b = new StringBuilder
      b.append(str)
      b.append(s)
      new BufferedStringHolder(b)
    }
    def clear { str = emptyString }
    def result = str
    def isEmpty : Boolean = false
  }

  class BufferedStringHolder(buf:StringBuilder) extends StringHolder {
    def +(s: CharSequence) = {
      buf.append(s)
      this
    }
    def clear { buf.clear() }
    def result = buf.result
    def isEmpty : Boolean = buf.isEmpty
  }


}