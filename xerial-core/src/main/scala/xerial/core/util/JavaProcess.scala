package xerial.core.util

import xerial.core.io.text.LineReader
import xerial.core.log.Logging

//--------------------------------------
//
// JavaProcess.scala
// Since: 2012/08/02 10:05
//
//--------------------------------------

case class JProcess(id:Int, name:String, cmd:String)


/**
 * @author leo
 */
object JavaProcess extends Logging {

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