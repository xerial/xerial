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
    val opt = CommandLauncher.of[Main]
    opt.execute(args)
  }
}

class Main extends CommandModule {
  val moduleName = "core"

  @command(description = "Set the log level of the JVM. Use jps to lookup JVM process IDs.")
  def loglevel(@option(symbol="l", description="logger name")
               loggerName:Option[String] = None,
               @argument(index=0, description="JVM process id. (use jps to see PIDs)")
               pid: Int,
               @argument(index=1, description="log level (all|trace|debug|info|warn|error|fatal|off)")
               logLevel:String) = {

    loggerName match {
      case Some(n) => LoggerFactory.setLogLevel(pid, n, logLevel)
      case None => LoggerFactory.setDefaultLogLevel(pid, logLevel)
    }
  }

}