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
// FastStringBuilderTest.scala
// Since: 2012/10/04 10:30 AM
//
//--------------------------------------

package xerial.core.io.text

import xerial.core.XerialSpec
import util.Random

/**
 * @author leo
 */
class FastStringBuilderTest extends XerialSpec {
  "FastStringBuilder" should {
    "create strings" in {
      val b = FastStringBuilder.newBuilder
      b += "hello world"
      b.result should be ("hello world")
      b.clear
      b.result should be ('empty)
    }

    "have no overhead in initialization" in {
      val s = Random.nextString(140)
      val t = time("initialization cost", repeat=5000) {
        block("FastStringBuilder", repeat=100) {
          val b = FastStringBuilder.newBuilder
          b += s
          b.result
        }

        block("StringBuilder", repeat=100) {
          val b = new StringBuilder
          b.append(s)
          b.result
        }
      }

      t("FastStringBuilder") should be <= (t("StringBuilder"))

    }


  }
}