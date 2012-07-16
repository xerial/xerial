//--------------------------------------
//
// Logger.scala
// Since: 2012/07/06 4:18 PM
//
//--------------------------------------

package xerial.core.util

import collection.mutable


/**
 * log level definitions
 *
 * @author Taro L. Saito
 */
object LogLevel {

  object OFF extends LogLevel(0, "off")
  object FATAL extends LogLevel(1, "fatal")
  object ERROR extends LogLevel(2, "error")
  object WARN extends LogLevel(3, "warn")
  object INFO extends LogLevel(4, "info")
  object DEBUG extends LogLevel(5, "debug")
  object TRACE extends LogLevel(6, "trace")
  object ALL extends LogLevel(7, "all")

  val values = Seq(OFF, FATAL, ERROR, WARN, INFO, DEBUG, TRACE, ALL)

  def apply(name: String): LogLevel = {
    val n = name.toLowerCase
    val lv = values.find(n == _.name)
    if (lv.isEmpty) {
      Console.err.println("Unknown log level [%s] Use info log level instead." format (name))
      INFO
    }
    else
      lv.get
  }
}

sealed abstract class LogLevel(val order: Int, val name: String) extends Ordered[LogLevel] {
  def compare(other: LogLevel) = this.order - other.order
}


/**
 * Adding log functions to your class.
 */
trait Logging extends LogHelper {

  private val logger = Logger(this.getClass)

  def log(logLevel: LogLevel, message: => Any) : Unit = {
    if (logger.isEnabled(logLevel))
      logger.log(logLevel, message)
  }

  /**
   * Create a sub logger with a given tag name
   * @param tag
   * @return
   */
  protected def logger(tag:Symbol) : Logger = Logger(logger, tag)

  /**
   * Create a sub logger with a given tag name
   * @param tag
   * @return
   */
  protected def logger(tag:String) : Logger = logger(Symbol(tag))

}

trait LogHelper {
  import LogLevel._
  def log(logLevel: LogLevel, message: => Any): Unit

  def fatal(message: => Any): Unit = log(FATAL, message)
  def error(message: => Any): Unit = log(ERROR, message)
  def warn(message: => Any): Unit = log(WARN, message)
  def info(message: => Any): Unit = log(INFO, message)
  def debug(message: => Any): Unit = log(DEBUG, message)
  def trace(message: => Any): Unit = log(TRACE, message)

  def fatal(format: String, a1: => Any): Unit = fatal(format.format(a1))
  def fatal(format: String, a1: => Any, a2: => Any): Unit = fatal(format.format(a1, a2))
  def fatal(format: String, a1: => Any, a2: => Any, a3: => Any): Unit = fatal(format.format(a1, a2, a3))
  def fatal(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Unit = fatal(format.format(a1, a2, a3, a4))
  def error(format: String, a1: => Any): Unit = error(format.format(a1))
  def error(format: String, a1: => Any, a2: => Any): Unit = error(format.format(a1, a2))
  def error(format: String, a1: => Any, a2: => Any, a3: => Any): Unit = error(format.format(a1, a2, a3))
  def error(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Unit = error(format.format(a1, a2, a3, a4))
  def warn(format: String, a1: => Any): Unit = warn(format.format(a1))
  def warn(format: String, a1: => Any, a2: => Any): Unit = warn(format.format(a1, a2))
  def warn(format: String, a1: => Any, a2: => Any, a3: => Any): Unit = warn(format.format(a1, a2, a3))
  def warn(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Unit = warn(format.format(a1, a2, a3, a4))
  def info(format: String, a1: => Any): Unit = info(format.format(a1))
  def info(format: String, a1: => Any, a2: => Any): Unit = info(format.format(a1, a2))
  def info(format: String, a1: => Any, a2: => Any, a3: => Any): Unit = info(format.format(a1, a2, a3))
  def info(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Unit = info(format.format(a1, a2, a3, a4))
  def debug(format: String, a1: => Any): Unit = debug(format.format(a1))
  def debug(format: String, a1: => Any, a2: => Any): Unit = debug(format.format(a1, a2))
  def debug(format: String, a1: => Any, a2: => Any, a3: => Any): Unit = debug(format.format(a1, a2, a3))
  def debug(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Unit = debug(format.format(a1, a2, a3, a4))
  def trace(format: String, a1: => Any): Unit = trace(format.format(a1))
  def trace(format: String, a1: => Any, a2: => Any): Unit = trace(format.format(a1, a2))
  def trace(format: String, a1: => Any, a2: => Any, a3: => Any): Unit = trace(format.format(a1, a2, a3))
  def trace(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Unit = trace(format.format(a1, a2, a3, a4))

}


object Logger {

  private def defaultLogLevel: LogLevel = LogLevel(System.getProperty("loglevel", "info"))

  private val rootLoggerName = "_"
  val rootLogger = new ConsoleLogger(rootLoggerName, defaultLogLevel)

  /**
   * Hold logger instances in weakly referenced hash map to allow releasing instances when necessary
   */
  private val loggerHolder = new mutable.WeakHashMap[String, Logger]


  def apply(cl: Class[_]): Logger = getLogger(cl.getName)
  def apply(name: String): Logger = getLogger(name)
  def apply(logger: Logger, symbol: Symbol): Logger = getLogger(logger.name + ":" + symbol.name)

  ///private def getLogLevel(name:String)

  /**
   * Get the logger of the specified name. LogWriter names are
   * dot-separated list of package names. LogWriter naming should be the same with java package/class naming convention.
   */
  private def getLogger(name: String): Logger = {

    def property(key:String) = Option(System.getProperty(key))

    def getLogLevel = {
      val l = property("loglevel:%s".format(name)) orElse {
         property("loglevel:%s".format(leafName(name)))
      } getOrElse defaultLogLevel.name
      LogLevel(l)
    }

    if (name.isEmpty)
      rootLogger
    else {
      synchronized {
        loggerHolder.getOrElseUpdate(name, new ConsoleLogger(name, getLogLevel))
      }
    }
  }

  def leafName(name:String) = name.split("""[\.]""").last

}


/**
 * Logger is
 * @author leo
 */
trait Logger extends LogHelper {

  val name: String
  val shortName = Logger.leafName(name)
  val tag = {
    val pos = shortName.lastIndexOf(":")
    if(pos == -1)
      Symbol("")
    else
      Symbol(shortName.substring(pos+1))
  }

  def logLevel : LogLevel

  def isEnabled(targetLogLevel: LogLevel): Boolean = targetLogLevel <= logLevel

  /**
   * Generate log message
   * @param level
   * @param message
   * @return true if log is generated, or false when log is suppressed
   */
  def log(level: LogLevel, message: => Any): Unit =  {
    if(isEnabled(level))
      write(level, message)
  }


  def write(level:LogLevel, message: Any)
}

/**
 * Empty logger
 */
trait NullLogger extends Logger {
  def write(level:LogLevel, message: Any) {}
}

/**
 * Logger for string messages
 */
trait StringLogger extends Logger {
  def write(level:LogLevel, message: Any) = write(level, message.toString)
  def write(level:LogLevel, message:String)
}


class ConsoleLogger(val name: String, var logLevel: LogLevel) extends StringLogger {

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


  def write(level: LogLevel, message: String) = {

    def isMultiLine(str: String) = str.contains("\n")
    val s = new StringBuilder

    def wrap(body: => Unit) = {
      s.append(colorPrefix(level))
      body
      s.append(Console.RESET)
    }

    wrap {
      s.append("[")
      s.append(shortName)
      s.append("] ")

      val m = message.toString
      if (isMultiLine(m))
        s.append("\n")
      s.append(m)
    }

    Console.err.println(s.toString)
  }
}







