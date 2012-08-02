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
    for{
      line <- scala.sys.process.Process("jps -v").lines.toSeq
      m <- jpsPattern.findFirstMatchIn(line)
    } yield {
      JProcess(m.group(1).toInt, m.group(2), m.group(3))
    }
  }

}