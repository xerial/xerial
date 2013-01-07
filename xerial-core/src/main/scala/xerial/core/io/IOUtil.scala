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
// IOUtil.scala
// Since: 2012/10/24 3:49 PM
//
//--------------------------------------

package xerial.core.io

import io.Source
import java.net.ServerSocket
import java.io.{File, ByteArrayOutputStream, InputStream}

/**
 * @author leo
 */
object IOUtil {
  def readFile[U](fileName:String)(f: Source => U) : U = {
    val source = Source.fromFile(fileName)
    try {
      f(source)
    }
    finally {
      //source.close
    }
  }

  def randomPort : Int = {
    val s = new ServerSocket(0)
    try {
      s.getLocalPort
    }
    finally {
      s.close
    }
  }

  def withResource[U, In <: java.io.Closeable](in:In)(f: In => U) : U = {
    try {
      f(in)
    }
    finally {
      if(in != null)
        in.close
    }
  }

  def readFully[U](in:InputStream)(f:Array[Byte] => U) : U = {

    val byteArray = withResource(new ByteArrayOutputStream) { b =>
      val buf = new Array[Byte](8192)
      withResource(in){ src =>
        var readBytes = 0
        while({readBytes = src.read(buf); readBytes != -1}) {
          b.write(buf, 0, readBytes)
        }
      }
      b.toByteArray
    }
    f(byteArray)
  }

  def ensureParentPath(f:File) {
    val parent = f.getParentFile
    if(parent != null && !parent.exists) {
      parent.mkdirs
    }
  }

}