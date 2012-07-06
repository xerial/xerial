//--------------------------------------
//
// Logger.scala
// Since: 2012/07/06 4:18 PM
//
//--------------------------------------

package xerial.core.util

import collection.mutable

/**
 * log level
 *
 * @author Taro L. Saito
 */
object LogLevel {

  object OFF extends LogLevel(0)
  object FATAL extends LogLevel(1)
  object ERROR extends LogLevel(2)
  object WARN extends LogLevel(3)
  object INFO extends LogLevel(4)
  object DEBUG extends LogLevel(5)
  object TRACE extends LogLevel(6)
  object ALL extends LogLevel(7)

  val values = Seq(OFF, FATAL, ERROR, WARN, INFO, DEBUG, TRACE, ALL)

  def apply(name: String): LogLevel = {
    val lv = values.find(name == _.name)
    if (lv.isEmpty) {
      Console.err.println("Unknown log level: %s. Use info log level instead." format (name))
      INFO
    }
    else
      lv.get
  }
}

sealed abstract class LogLevel(val order: Int) extends Ordered[LogLevel] {
  val name = this.getClass.getSimpleName.toLowerCase

  def compare(other: LogLevel) = this.order - other.order
}

/**
 * Adding log functions to your class.
 */
trait Logging {

  protected val _logger: Logger = Logger(this.getClass)

  import LogLevel._

  protected def fatal(message: => Any): Boolean = log(FATAL, message)
  protected def error(message: => Any): Boolean = log(ERROR, message)
  protected def warn(message: => Any): Boolean = log(WARN, message)
  protected def info(message: => Any): Boolean = log(INFO, message)
  protected def debug(message: => Any): Boolean = log(DEBUG, message)
  protected def trace(message: => Any): Boolean = log(TRACE, message)

  protected def log(logLevel: LogLevel, message: => Any): Boolean = {
    if (_logger.isEnabled(logLevel)) {
      _logger.log(logLevel, message)
      true
    }
    else
      false
  }


}

object Logger {

  private def defaultLogLevel: LogLevel = LogLevel(Option(System.getProperty("loglevel")).getOrElse("info"))

  private val rootLoggerName = "_root_"
  val rootLogger = new StandardLogger(rootLoggerName, defaultLogLevel)

  /**
   * Hold logger instances in weakly referenced hash map to allow releasing instances when necessary
   */
  private val loggerHolder = new mutable.WeakHashMap[String, Logger]


  def apply(cl: Class[_]): Logger = getLogger(cl)

  def getLogger(cl: Class[_]): Logger = {
    getLogger(cl.getName())
  }

  /**
   * Get the logger of the specified name. LogWriter names are
   * dot-separated list of package names. LogWriter naming should be the same with java package/class naming convention.
   */
  def getLogger(name: String): Logger = {
    if (name.isEmpty)
      rootLogger
    else
      loggerHolder(name)
  }

  private def parentLoggerName(name: String): String = {
    val p = name.split( """\.""")
    if (p.isEmpty)
      Logger.rootLoggerName
    else
      p.slice(0, p.length - 1).mkString(".")
  }

}


/**
 * @author leo
 */
trait Logger {

  val name : String
  val shortName = name.split( """\.""").last
  protected var logLevel: LogLevel

  def isEnabled(targetLogLevel: LogLevel): Boolean = targetLogLevel <= logLevel

  def log(level: LogLevel, message: => Any): Unit

}


class StandardLogger(val name: String, var logLevel: LogLevel) extends Logger {

  import LogLevel._

  protected val colorPrefix = Map[LogLevel, String](
    ALL -> "",
    TRACE -> Console.GREEN,
    DEBUG -> Console.WHITE,
    INFO -> Console.CYAN,
    WARN -> Console.YELLOW,
    ERROR -> Console.MAGENTA,
    FATAL -> Console.RED,
    OFF -> "")


  def log(level: LogLevel, message: => Any) {
    def isMultiLine(str: String) = str.contains("\n")
    val s = new StringBuilder

    def wrap(body:  => Unit) = {
      s.append(colorPrefix(level))
      body
      s.append(Console.RESET)
    }

    wrap {
      s.append("[")
      s.append(name)
      s.append("] ")

      val m = message.toString
      if (isMultiLine(m))
        s.append("\n")
      s.append(m)
    }

    Console.err.println(s.toString)
  }
}







