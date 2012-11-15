//--------------------------------------
//
// Main.scala
// Since: 2012/07/20 2:41 PM
//
//--------------------------------------

package xerial.cui

import xerial.core.log.{LoggerFactory}


/**
 * @author leo
 */
object Main {
  def main(args:Array[String]) : Unit = {
    Launcher.execute[Main](args)
  }
}

class Main {

  @command(description = "Set the log level of the JVM. Use jps to lookup JVM process IDs.")
  def loglevel(@option(prefix="-l", description="logger name")
               loggerName:Option[String] = None,
               @argument(description="JVM process id. (use jps to see PIDs)")
               pid: Int,
               @argument(description="log level (all|trace|debug|info|warn|error|fatal|off)")
               logLevel:String) = {

    loggerName match {
      case Some(n) => LoggerFactory.setLogLevel(pid, n, logLevel)
      case None => LoggerFactory.setDefaultLogLevel(pid, logLevel)
    }
  }

}