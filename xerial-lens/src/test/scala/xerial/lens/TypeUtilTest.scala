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
// TypeUtilTest.scala
// Since: 2012/11/06 11:01 AM
//
//--------------------------------------

package xerial.lens

import xerial.core.XerialSpec
import xerial.core.log.{Logger, LogLevel}

/**
 * @author leo
 */
class TypeUtilTest extends XerialSpec
{
  "TypeConverter" should {
    "Create singleton object from name" in {
      for(l <- LogLevel.values) {
        val m = TypeConverter.convert(l.name, classOf[LogLevel]) getOrElse None
        m should be (l)
      }
    }
  }
}

class A(val flag:Option[LogLevel]=None) extends Logger