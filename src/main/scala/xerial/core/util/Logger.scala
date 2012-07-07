//--------------------------------------
//
// Logger.scala
// Since: 2012/07/06 4:18 PM
//
//--------------------------------------

package xerial.core.util

import collection.mutable
import mutable.Stack


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


trait LoggerHolder {
  def current: Logger
  def push(symbol: Symbol): LoggerHolder
  def pop: LoggerHolder
}

class LoggerHolder0(val current: Logger) extends LoggerHolder {
  def this(loggerName: String) = this(LoggerFactory(loggerName))
  def push(symbol: Symbol) = {
    new LoggerHolder1(current, symbol)
  }
  def pop = sys.error("cannot pop from empty logger holder: %s".format(current.name))
}


class LoggerHolder1(logger: Logger, symbol: Symbol) extends LoggerHolder {
  private val stack = Stack[Logger]()
  stack.push(logger)
  stack.push(LoggerFactory(logger, symbol))

  def push(symbol: Symbol) = {
    stack.push(LoggerFactory(current, symbol))
    this
  }
  def pop = {
    stack.pop
    if (stack.size == 1)
      new LoggerHolder0(logger)
    else
      this
  }

  protected def path: String = {
    if (stack.isEmpty)
      "/"
    else
      stack.reverse.mkString("/")
  }

  def current: Logger = stack.top

}


/**
 * Adding log functions to your class.
 */
trait Logging {

  import LogLevel._

  private var _loggerHolder: LoggerHolder = new LoggerHolder0(this.getClass.getName)

  protected def log(logLevel: LogLevel, message: => Any): Boolean = {
    val current = _loggerHolder.current
    if (current.isEnabled(logLevel)) {
      current.log(logLevel, message)
      true
    }
    else
      false
  }

  protected def log(tag: Symbol)(body: => Unit) {
    val currentTag = _loggerHolder.current.tag
    if(currentTag == tag)
      body
    else {
      _loggerHolder = _loggerHolder.push(tag)
      try
        body
      finally
        _loggerHolder = _loggerHolder.pop
    }
  }

  protected def fatal(message: => Any): Boolean = log(FATAL, message)
  protected def error(message: => Any): Boolean = log(ERROR, message)
  protected def warn(message: => Any): Boolean = log(WARN, message)
  protected def info(message: => Any): Boolean = log(INFO, message)
  protected def debug(message: => Any): Boolean = log(DEBUG, message)
  protected def trace(message: => Any): Boolean = log(TRACE, message)

  protected def fatal(format: String, a1: => Any): Boolean = fatal(format.format(a1))
  protected def fatal(format: String, a1: => Any, a2: => Any): Boolean = fatal(format.format(a1, a2))
  protected def fatal(format: String, a1: => Any, a2: => Any, a3: => Any): Boolean = fatal(format.format(a1, a2, a3))
  protected def fatal(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Boolean = fatal(format.format(a1, a2, a3, a4))
  protected def error(format: String, a1: => Any): Boolean = error(format.format(a1))
  protected def error(format: String, a1: => Any, a2: => Any): Boolean = error(format.format(a1, a2))
  protected def error(format: String, a1: => Any, a2: => Any, a3: => Any): Boolean = error(format.format(a1, a2, a3))
  protected def error(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Boolean = error(format.format(a1, a2, a3, a4))
  protected def warn(format: String, a1: => Any): Boolean = warn(format.format(a1))
  protected def warn(format: String, a1: => Any, a2: => Any): Boolean = warn(format.format(a1, a2))
  protected def warn(format: String, a1: => Any, a2: => Any, a3: => Any): Boolean = warn(format.format(a1, a2, a3))
  protected def warn(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Boolean = warn(format.format(a1, a2, a3, a4))
  protected def info(format: String, a1: => Any): Boolean = info(format.format(a1))
  protected def info(format: String, a1: => Any, a2: => Any): Boolean = info(format.format(a1, a2))
  protected def info(format: String, a1: => Any, a2: => Any, a3: => Any): Boolean = info(format.format(a1, a2, a3))
  protected def info(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Boolean = info(format.format(a1, a2, a3, a4))
  protected def debug(format: String, a1: => Any): Boolean = debug(format.format(a1))
  protected def debug(format: String, a1: => Any, a2: => Any): Boolean = debug(format.format(a1, a2))
  protected def debug(format: String, a1: => Any, a2: => Any, a3: => Any): Boolean = debug(format.format(a1, a2, a3))
  protected def debug(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Boolean = debug(format.format(a1, a2, a3, a4))
  protected def trace(format: String, a1: => Any): Boolean = trace(format.format(a1))
  protected def trace(format: String, a1: => Any, a2: => Any): Boolean = trace(format.format(a1, a2))
  protected def trace(format: String, a1: => Any, a2: => Any, a3: => Any): Boolean = trace(format.format(a1, a2, a3))
  protected def trace(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Boolean = trace(format.format(a1, a2, a3, a4))

}


object LoggerFactory {

  private def defaultLogLevel: LogLevel = LogLevel(Option(System.getProperty("loglevel")).getOrElse("info"))

  private val rootLoggerName = "_"
  val rootLogger = new StandardLogger(None, rootLoggerName, defaultLogLevel)

  /**
   * Hold logger instances in weakly referenced hash map to allow releasing instances when necessary
   */
  private val loggerHolder = new mutable.WeakHashMap[String, Logger]


  def apply(cl: Class[_]): Logger = getLogger(cl.getName)
  def apply(name: String): Logger = getLogger(name)
  def apply(logger: Logger, symbol: Symbol): Logger = getLogger(logger.name + ":" + symbol.name)

  /**
   * Get the logger of the specified name. LogWriter names are
   * dot-separated list of package names. LogWriter naming should be the same with java package/class naming convention.
   */
  def getLogger(name: String): Logger = {
    if (name.isEmpty)
      rootLogger
    else
      loggerHolder.getOrElseUpdate(name, new StandardLogger(loggerHolder.get(parentLoggerName(name)), name, defaultLogLevel))
  }
  

  private def parentLoggerName(name: String): String = {
    val p = name.split("""\.""")
    if (p.isEmpty)
      rootLoggerName
    else
      p.slice(0, p.length - 1).mkString(".")
  }

}


/**
 * @author leo
 */
trait Logger {

  val name: String
  val shortName = name.split("""[\.]""").last
  val tag = {
    val pos = shortName.lastIndexOf(":")
    if(pos == -1)
      Symbol("")
    else
      Symbol(shortName.substring(pos+1))
  }
  protected var logLevel: LogLevel

  protected val parent: Option[Logger]

  def isEnabled(targetLogLevel: LogLevel): Boolean = targetLogLevel <= logLevel

  def log(level: LogLevel, message: => Any): Unit

}


class StandardLogger(protected val parent: Option[Logger], val name: String, var logLevel: LogLevel) extends Logger {

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







