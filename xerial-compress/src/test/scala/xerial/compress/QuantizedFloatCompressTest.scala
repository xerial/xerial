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
// QuantizedFloatCompressTest.scalaest.scala
// Since: 2012/12/07 10:33 AM
//
//--------------------------------------

package xerial.compress

import xerial.core.XerialSpec
import org.xerial.snappy.Snappy
import util.Random

/**
 * @author Taro L. Saito
 */
class QuantizedFloatCompressTest extends XerialSpec {

  "QuantizedFloatCompress" should {
    "compress [-1, 1] values" in {
      val N = 100000
      def sample = {
        (for(i <- 0 until N) yield {
          Random.nextFloat / Float.MaxValue
          //math.sin(math.toRadians(i)).toFloat
        }).toArray[Float]
      }

      val in = sample
      var fc : Array[Byte] = null
      var sc : Array[Byte] = null

      time("float compression", repeat=10) {
        block("quantized") {
          fc = QuantizedFloatCompress.compress(in)
        }
        block("snappy") {
          sc = Snappy.compress(in)
        }
      }

      debug("quantized: compressed size: %,d => %,d", in.length*8, fc.length)
      debug("snappy   : compressed size: %,d => %,d", in.length*8, sc.length)

      var fcDecompressed : Array[Float] = null
      var scDecompressed : Array[Float] = null

      time("float decompression", repeat=10) {
        block("quantized") {
          fcDecompressed = QuantizedFloatCompress.decompress(fc)
        }

        block("snappy") {
          val a : Array[Byte] = Snappy.uncompress(sc)
          scDecompressed = new Array[Float](a.length / 4)
          Snappy.arrayCopy(a, 0, a.length, scDecompressed, 0)
        }
      }
      val error = in.zip(fcDecompressed).find{case (a, b) => (a - b).abs > 0.00000001 }
      error map { e =>
        fail("has distinct values more than 0.00000001 after decompression: %s".format(e))
      }
//      for(i <- 0 until N) {
//        println("%s\t%s\tdiff:%s".format(in(i), decompressed(i), (in(i) - decompressed(i)).abs))
//      }

    }

    "compress extream values" in {
      val ans : Array[Float] = Array(0.0f, 1.0f, -1.0f, 0.5f, -0.5f, 0.1f, 0.2f, -0.1f, 0.2f)
      val compressed = QuantizedFloatCompress.compress(ans)
      val decompressed : Array[Float] = QuantizedFloatCompress.decompress(compressed)
      ans should be (decompressed)
    }
  }
}