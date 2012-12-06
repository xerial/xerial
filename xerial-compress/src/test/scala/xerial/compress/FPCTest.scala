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
// FPCTest.scala
// Since: 2012/12/06 12:50 PM
//
//--------------------------------------

package xerial.compress

import xerial.core.XerialSpec

/**
 * @author Taro L. Saito
 */
class FPCTest extends XerialSpec {
  "FPC" should {
    "compress Double arrays" in {

      // Create a sin curve data
      val N = 1024 // data size
      val input = (for(i <- 0 until N) yield {
        //(i % 3).toDouble
        math.sin(i * 0.1)
      }).toArray[Double]

      val compressed = FPC.compress(input)
      debug("compressed size: %,d => %,d", input.length * 8, compressed.length)

      val decompressed = FPC.decompress(compressed)


      val equal = input.zipAll(decompressed, 0.0, 0.0).forall(x => x._1 == x._2)
      equal should be (true)
    }


  }
}