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
    "support append/prepend" in {
      val c = new CyclicArray[Int]
      c.append(1)
      c.append(3)
      c.prepend(10)
      c.prepend(20)

      c should have size (4)
      c should be (Seq(20, 10, 1, 3))

      c.removeFirst should be (Seq(10, 1, 3))
      c.removeLast should be (Seq(10, 1))
      c.size should be (2)
    }

    "be capable to size expansion" in {
      val c = new CyclicArray[Int]
      for(i <- 0 until 10) {
        c.append(i)
        c.prepend(i)
      }

      c.toSeq should be (Seq(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9))

    }

    "have a constructor" in {

      val c = CyclicArray(0, 20, 40, 44)
      c should be (Seq(0, 20, 40, 44))

    }


  }
}