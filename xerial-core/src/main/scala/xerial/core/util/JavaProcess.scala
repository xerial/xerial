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

package xerial.core.util

import xerial.core.io.text.LineReader
import xerial.core.log.Logger

//-------------------------------------
//
// JavaProcess.scala
// Since: 2012/08/02 10:05
//
//--------------------------------------

case class JProcess(id:Int, name:String, cmd:String)


/**
 * @author leo
 */
object JavaProcess extends Logger {

  private val jpsPattern = """([0-9]+)\s+(\S+)\s+(\S.*)""".r

  /**
   * Obtain a list of java processes running in this machine
   * @return
   */
  def list : Iterable[JProcess] = {
    val cmd = Shell.findJavaCommand("jps")
    if(cmd.isEmpty)
      sys.error("no jps command is found. Set PATH or JAVA_HOME properly.")
    else
      for{
        line <- scala.sys.process.Process("%s -v".format(cmd.get)).lines.toSeq
        m <- jpsPattern.findFirstMatchIn(line)
      } yield  JProcess(m.group(1).toInt, m.group(2), m.group(3))
  }

}

