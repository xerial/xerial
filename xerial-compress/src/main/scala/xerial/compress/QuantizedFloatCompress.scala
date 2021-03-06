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
// QuantizedFloatCompress.scala
// Since: 2012/12/07 10:21 AM
//
//--------------------------------------

package xerial.compress

import xerial.core.log.Logger
import java.nio.ByteOrder
import org.xerial.snappy.Snappy


/**
 * Fast compression for [-1, 1] float values
 *
 * @author Taro L. Saito
 */
object QuantizedFloatCompress extends Logger {

  /**
   * Translate [-1, 1] floating point values into Int values within [Int.MinValue, Int.Maxvalue] range
   * @param in input data
   * @return quantized data
   */
  def quantizeToInt(in:Array[Float]) : Array[Int] = {
    val N = in.length
    var c = 0
    val out = new Array[Int](N)
    while(c < N) {
      val v : Float = in(c)
      if(v.abs > 1.0) {
        throw new IllegalArgumentException("the data contains illegal value |v| = |%.2f| > 1 at %d".format(v, c))
      }
      val vi = (v * Int.MaxValue).toInt
      // write Int using the native endian
      out(c) = vi
      c += 1
    }
    out
  }

  /**
   * Translate [-1, 1] floating point values into Byte values within [Byte.MinValue, Byte.Maxvalue] range
   * @param in input data
   * @return quantized data
   */
  def quantizeToByte(in:Array[Float]) : Array[Byte] = {
    val N = in.length
    var c = 0
    val out = new Array[Byte](N)
    while(c < N) {
      val v : Float = in(c)
      if(v.abs > 1.0) {
        throw new IllegalArgumentException("the data contains illegal value |v| = |%.2f| > 1 at %d".format(v, c))
      }

      val vi = (v * Byte.MaxValue).toByte
      // write byte
      out(c) = vi
      c += 1
    }
    out
  }

  /**
   * Compress [-1, 1] floating point values by quantizing them into Int values within [Int.MinValue, Int.Maxvalue]
   * @param in input data
   * @return compressed data
   */
  def compress(in:Array[Float]) : Array[Byte] = Snappy.compress(quantizeToInt(in))

  /**
   * Decompress float values compressed with [[xerial.compress.QuantizedFloatCompress#compress]]
   * @param in
   * @return decompressed float array
   */
  def decompress(in:Array[Byte]) : Array[Float] = {
    val ia : Array[Byte] = Snappy.uncompress(in)
    val out = new Array[Float](ia.length / 4)
    var c = 0
    val N = ia.length
    val isBigEndian = ByteOrder.nativeOrder() eq ByteOrder.BIG_ENDIAN
    while(c < N) {
      val vi = if(isBigEndian) {
        ((ia(c) & 0xFF) << 24) |
          ((ia(c+1) & 0xFF) << 16) |
          ((ia(c+2) & 0xFF) << 8) |
          (ia(c+3) & 0xFF)
      }
      else {
        (ia(c) & 0xFF) |
          ((ia(c+1) & 0xFF) << 8) |
          ((ia(c+2) & 0xFF) << 16) |
          ((ia(c+3) & 0xFF) << 24)
      }
      val v = vi.toFloat / Int.MaxValue
      out(c >>> 2) = v
      c += 4
    }
    out
  }

  /**
   * Compress float values within [-1, 1] by quantizing them as Byte values [-128, 127]
   * @param in
   * @return
   */
  def compressAsByte(in:Array[Float]) : Array[Byte] = Snappy.compress(quantizeToByte(in))


  /**
   * Decompress float values compressed with [[xerial.compress.QuantizedFloatCompress#compressAsByte]]
   * @param in
   * @return
   */
  def decompressAsByte(in:Array[Byte]) : Array[Float] = {
    val ia : Array[Byte] = Snappy.uncompress(in)
    var c = 0
    val N = ia.length
    val out : Array[Float] = new Array[Float](N)
    while(c < N) {
      val vi = ia(c)
      val v = vi.toFloat / Byte.MaxValue
      out(c) = v
      c += 1
    }
    out
  }



}