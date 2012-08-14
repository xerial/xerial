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

package xerial.core.collection

import xerial.core.XerialSpec

//--------------------------------------
//
// CyclicArrayTest.scala
// Since: 2012/08/14 9:57
//
//--------------------------------------

/**
 * @author Taro L. Saito
 */
class CyclicArrayTest extends XerialSpec {
  "CyclicArray" should {
    "support append" in {
      val c = new CyclicArray[Int]
      c.addLast(1)
      c.addLast(3)

      debug(c.mkString(", "))

    }
  }
}