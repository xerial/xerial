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
// FPC.scala
// Since: 2012/12/06 12:46 PM
//
//--------------------------------------

package xerial.compress


import java.nio.ByteBuffer
import xerial.core.log.Logger

/**
 * An implementation of the FPC 64-bit floating point value compression algorithm:
 * M. Burtscher and P. Ratanaworaban. High Troughput Compression of Double-Precision Floating-Point Data. DCC2007
 * http://csl.cornell.edu/~burtscher/research/FPC
 *
 *
 * @author Taro L. Saito
 */
object FPC extends Logger {

  def compress(input:Array[Double], tableSize:Int = 8 * 1024) : Array[Byte] = {

    val hashTableSizeInLog2 = math.max(4, (32 - Integer.numberOfLeadingZeros(tableSize)))  // truncate to 2^i value
    val hashTableSize = 1 << hashTableSizeInLog2

    trace("hash table size: %,d", hashTableSize)
    val tableMask = hashTableSize - 1
    trace("table mask:%s", (for(i <- 0 until 32) yield { if(((1 << i) & tableMask) == 0) "1" else "0"}).mkString)
    val fcm = new Array[Long](hashTableSize)
    val dfcm = new Array[Long](hashTableSize)

    val N = input.length
    //val M = 1024  // num elements in a block
    // output buffer that has enough size for compressing M Double elements
    val buf = new Array[Byte](6 + ((N + 1)/ 2) + (N * 8))
    val bufOut = ByteBuffer.wrap(buf)

    // write hash table size (1 byte)
    bufOut.put(hashTableSizeInLog2.toByte)
    // write element size (4 bytes)
    bufOut.putInt(N)

    var residualOffset = 6 + ((N + 1)/ 2)
    var pred1 = 0L
    var pred2 = 0L
    var c  = 0
    var last = 0L
    var hash1 = 0
    var hash2 = 0
    while(c < N) {
      // Read Double value as Long
      val v = java.lang.Double.doubleToRawLongBits(input(c))
      // FCM
      @inline def predWithFCM : Long = {
        val xor = v ^ pred1 // Take XOR with the prediction
        fcm(hash1) = v       // Update the hash table with the current value
        // Predict the next value
        hash1 = ((hash1 << 6) ^ (v >>> 48L).toInt) & tableMask
        pred1 = fcm(hash1)
        xor
      }
      // dFCM
      @inline def predWithDFCM : Long = {
        val diff = v - last
        dfcm(hash2) = diff   // Update the hash table
        val xor = v ^ (last + pred2) // Take XOR with the prediction
        // Predict the next value
        hash2 = ((hash2 << 2) ^ (diff >>> 40L).toInt) & tableMask
        pred2 = dfcm(hash2)
        xor
      }

      @inline def calcBCode(xor:Long) : Int = {
        // We give up using 4 bytes as a block size since its frequency is quite low
        var bcode = 7 // 8 bytes
        if((xor >> 56) == 0) bcode = 6  // 7 bytes
        if((xor >> 48) == 0) bcode = 5 // 6 bytes
        if((xor >> 40) == 0) bcode = 4 // 5 bytes
        if((xor >> 24) == 0) bcode = 3 // 3 bytes
        if((xor >> 16) == 0) bcode = 2 // 2 bytes
        if((xor >> 8) == 0) bcode = 1 // 1 byte
        if(xor == 0) bcode = 0 // 0 byte
        bcode
      }

      val xor1 = predWithFCM
      val xor2 = predWithDFCM

      var code : Int = 0
      var xor : Long = xor1
      if(xor1 > xor2) {
        code = 0x8
        xor = xor2
      }
      // bcode encodes the residual block size.
      val bcode = calcBCode(xor)
      code |= bcode
      val pos = 6 + (c >> 1)
      buf(pos) = (buf(pos) | (code << ((1 - (c & 1)) << 2))).toByte
      //debug("write code:%d at %d buf(%d) = %d", code, pos, pos, buf(pos))
      val residualSize = bcode + (bcode >> 2) // The last term is a compensation for missing 4 bytes code
      //debug("rsize: %d", residualSize)
      //debug("residual size: %d, v:%d, pred1:%d, pred2:%d, nlz:%d".format(residualSize, v, pred1, pred2, java.lang.Long.numberOfLeadingZeros(xor)))
      debug("write xor:%s, bcode:%d, r:%d", java.lang.Long.toHexString(xor), bcode, residualSize)
      var i = 0
      while(i < residualSize) {
        val vi = ((xor >>> ((residualSize - i - 1) << 3)) & 0xFF).toByte
        buf(residualOffset + i) = vi
        //debug("write(%d): %s", residualOffset + i, java.lang.Long.toHexString(vi))
        i += 1
      }
      residualOffset += residualSize
      last = v
      c += 1
    }

    val compressed = new Array[Byte](residualOffset)
    Array.copy(buf, 0, compressed, 0, compressed.length)
    compressed
  }

  private val bcodeMask = Array(
    0L,   // 0 byte
    ~0L >>> 56L,  // 1 byte
    ~0L >>> 48L, // 2 bytes
    ~0L >>> 40L, // 3 bytes
    ~0L >>> 24L, // 5 bytes
    ~0L >> 16L, // 6 bytes
    ~0L >> 8L, // 7 bytes
    ~0L // 8 bytes
  )

  def decompress(compressed:Array[Byte]) : Array[Double] = {

    val in = ByteBuffer.wrap(compressed)
    val hashTableSizeInLog2 = in.get()
    val N = in.getInt
    val hashTableSize = 1 << hashTableSizeInLog2
    val tableMask = hashTableSize - 1
    val fcm = new Array[Long](hashTableSize)
    val dfcm = new Array[Long](hashTableSize)

    val decompressed = new Array[Double](N)

    var c = 0
    var residualOffset = 6 + ((N +1) / 2)
    var pred1 = 0L
    var pred2 = 0L
    var last = 0L
    var hash1 = 0
    var hash2 = 0
    while(c < N) {
      val pos = 6 + (c >> 1)
      val code = compressed(pos) >>> ((1 - (c & 1)) << 2) & 0x0F
      val bcode = code & 0x7
      //debug("read code: %d at %d buf(%d) = %d" , code, pos, pos, compressed(pos))
      val residualSize = bcode + (bcode >> 2)

      //in.position(residualOffset)
      //var v = in.getLong >>> ((8 - residualSize) * 8)
      var xor = 0L
      var r = 0
      while(r < residualSize) {
        xor <<= 8L
        val vi : Long = compressed(residualOffset + r) & 0xFF
        xor |= vi
        //debug("read(%d): %s, v:%s", residualOffset + r, java.lang.Long.toHexString(vi), java.lang.Long.toHexString(v))
        r += 1
      }
      val pred = if((code & 0x7) == 0) pred1 else pred2
      debug("read xor:%s, bcode:%d, r:%d", java.lang.Long.toHexString(xor), bcode, residualSize)
      val v = xor ^ pred

      fcm(hash1) = v
      hash1 = ((hash1 << 6) ^ (v >>> 48L).toInt) & tableMask
      pred1 = fcm(hash1)

      val diff = v - last
      dfcm(hash2) = diff
      hash2 = ((hash2 << 2) ^ (diff >>> 40L).toInt) & tableMask
      pred2 = v + dfcm(hash2)

      decompressed(c) = java.lang.Double.longBitsToDouble(v)

      last = v
      residualOffset += residualSize
      c += 1
    }

    decompressed
  }

}