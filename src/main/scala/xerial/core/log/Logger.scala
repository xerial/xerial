//--------------------------------------
//
// Logger.scala
// Since: 2012/07/19 4:23 PM
//
//--------------------------------------

package xerial.core.log

import collection.mutable
import javax.management.{MBeanServerConnection, JMX, ObjectName}
import management.ManagementFactory
import javax.management.remote.{JMXConnectorFactory, JMXServiceURL}

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
  override def toString = name
}

/**
 * Adding log functions to your class.
 */
trait Logging extends LogHelper {

  private[this] val logger = Logger(this.getClass)

  def log(logLevel: LogLevel, message: => Any): Unit = {
    if (logger.isEnabled(logLevel))
      logger.log(logLevel, message)
  }

  /**
   * Create a sub logger with a given tag name
   * @param tag
   * @return
   */
  protected def getLogger(tag: Symbol): Logger = Logger(logger, tag)

  /**
   * Create a sub logger with a given tag name
   * @param tag
   * @return
   */
  protected def getLogger(tag: String): Logger = getLogger(Symbol(tag))

  protected def log[U](tag: String)(f: Logger => U) {
    f(getLogger(tag))
  }

}




/**
 * Add support for formatted log
 */
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


/**
 * Logger interface
 * @author leo
 */
trait Logger extends LogHelper {

  val name: String
  val shortName = Logger.leafName(name)
  val tag = {
    val pos = shortName.lastIndexOf(":")
    if (pos == -1)
      Symbol("")
    else
      Symbol(shortName.substring(pos + 1))
  }

  var logLevel: LogLevel

  def isEnabled(targetLogLevel: LogLevel): Boolean = targetLogLevel <= logLevel

  /**
   * Generate log message
   * @param level
   * @param message
   * @return true if log is generated, or false when log is suppressed
   */
  def log(level: LogLevel, message: => Any): Unit = {
    if (isEnabled(level))
      write(level, message)
  }


  protected def write(level: LogLevel, message: Any)
}


object Logger {

  private[log] var defaultLogLevel: LogLevel = LogLevel(System.getProperty("loglevel", "info"))

  private val rootLoggerName = "root"
  val rootLogger = new ConsoleLogger(rootLoggerName, defaultLogLevel)

  /**
   * Hold logger instances in weakly referenced hash map to allow releasing instances when necessary
   */
  private val loggerHolder = new mutable.WeakHashMap[String, Logger]


  def apply(cl: Class[_]): Logger = getLogger(cl.getName)
  def apply(name: String): Logger = getLogger(name)
  def apply(logger: Logger, symbol: Symbol): Logger = getLogger(logger.name + ":" + symbol.name)
  def apply(cl: Class[_], symbol: Symbol): Logger = apply(apply(cl), symbol)


  /**
   * Get the logger of the specified name. LogWriter names are
   * dot-separated list of package names. LogWriter naming should be the same with java package/class naming convention.
   */
  private def getLogger(name: String): Logger = {

    def property(key: String) = Option(System.getProperty(key))

    def getLogLevel = {
      val l = property("loglevel:%s".format(leafName(name))) orElse {
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

  def leafName(name: String) = name.split( """[\.]""").last

  private val configMBeanName = new ObjectName("xerial.core.util:type=LoggerConfig")

  {
    val server = ManagementFactory.getPlatformMBeanServer
    try {
      if(!server.isRegistered(configMBeanName))
        server.registerMBean(new LoggerConfigImpl, configMBeanName)
    }
    catch {
      case e: Exception => e.printStackTrace()
    }
  }



  def setLogLevelJMX(loggerName:String, logLevel:String) {
    val lc = JMX.newMBeanProxy(ManagementFactory.getPlatformMBeanServer, configMBeanName, classOf[LoggerConfig], true)
    lc.setLogLevel(loggerName, logLevel)
  }

  def setLogLevelJMX(server:MBeanServerConnection, loggerName:String, logLevel:String) {
    val lc = JMX.newMBeanProxy(server, configMBeanName, classOf[LoggerConfig], true)
    lc.setLogLevel(loggerName, logLevel)
    rootLogger.info("Set the loglevel of %s to %s", loggerName, logLevel)
  }
  def setDefaultLogLevelJMX(server:MBeanServerConnection, logLevel:String) {
    val lc = JMX.newMBeanProxy(server, configMBeanName, classOf[LoggerConfig], true)
    lc.setDefaultLogLevel(logLevel)
    rootLogger.info("Set the default loglevel to %s", logLevel)
  }


  def getJMXServerAddress(pid:Int) : Option[String] = {
    Option(sun.management.ConnectorAddressLink.importFrom(pid))
  }

  /**
   *
   */
  private def getJMXServer(pid:Int) : Option[MBeanServerConnection] = {
    rootLogger.info("Searching for JMX server pid:%d", pid)
    val server = getJMXServerAddress(pid).map { addr =>
      JMXConnectorFactory.connect(new JMXServiceURL(addr))
    } map (_.getMBeanServerConnection)

    if(server.isEmpty)
      rootLogger.warn("No JMX server (pid:%d) is found", pid)
    else
      rootLogger.info("Found a JMX server pid:%d", pid)
    server
  }


  def setLogLevel(pid:Int, loggerName:String, logLevel:String) {
    for(server <- getJMXServer(pid)) {
      setLogLevelJMX(server, loggerName, logLevel)
    }
  }

  def setDefaultLogLevel(pid:Int, logLevel:String) {
    for(server <- getJMXServer(pid)) {
      setDefaultLogLevelJMX(server, logLevel)
    }
  }


  def main(args:Array[String]) {
    def at(index:Int) : Option[String] = {
      if(args.isDefinedAt(index))
        Some(args(index))
      else
        None
    }

    // Resolve MBean port number
    for(pid <- at(0); loggerName <- at(1); logLevel <- at(2)) {
      setLogLevel(pid.toInt, loggerName, logLevel)
    }
  }

}

import javax.management.MXBean
/**
 * Logger configuration API
 * @author leo
 */
@MXBean abstract trait LoggerConfig {
  def setLogLevel(name: String, logLevel: String): Unit
  def setDefaultLogLevel(logLevel:String) : Unit
  def getDefaultLogLevel : String
}


class LoggerConfigImpl extends LoggerConfig {

  def getDefaultLogLevel = Logger.defaultLogLevel.toString

  def setLogLevel(loggerName: String, logLevel: String)  {
    System.setProperty("loglevel:%s".format(loggerName), logLevel)
    val logger = Logger.apply(loggerName)
    val level = LogLevel(logLevel)
    logger.logLevel = level
    Logger.rootLogger.info("set the log level of %s to %s", loggerName, level)
  }

  def setDefaultLogLevel(logLevel:String) {
    val level = LogLevel(logLevel)
    System.setProperty("loglevel", level.toString)
    Logger.rootLogger.info("set the default log level to %s", level)
  }
}



/**
 * Empty logger
 */
trait NullLogger extends Logger {
  protected def write(level: LogLevel, message: Any) {}
}

/**
 * Logger for string messages
 */
trait StringLogger extends Logger {
  def write(level: LogLevel, message: Any) = write(level, formatLog(level, message))
  /**
   * Override this method to customize the log format
   * @param message
   * @return
   */
  protected def formatLog(logLevel:LogLevel, message: Any): String = {

    def isMultiLine(str: String) = str.contains("\n")
    val s = new StringBuilder

    s.append("[")
    s.append(shortName)
    s.append("] ")

    val m = message match {
      case null => ""
      case _ => message.toString
    }
    if (isMultiLine(m))
      s.append("\n")
    s.append(m)

    s.toString
  }

  protected def write(level: LogLevel, message: String) = Console.err.println(message)
}

object ConsoleLogger {

  import LogLevel._
  val colorPrefix = Map[LogLevel, String](
    ALL -> "",
    TRACE -> Console.GREEN,
    DEBUG -> Console.WHITE,
    INFO -> Console.CYAN,
    WARN -> Console.YELLOW,
    ERROR -> Console.MAGENTA,
    FATAL -> Console.RED,
    OFF -> "")

}

class ConsoleLogger(val name: String, var logLevel: LogLevel) extends StringLogger {

  override protected def formatLog(level:LogLevel, message: Any) = {
    "%s%s%s".format(ConsoleLogger.colorPrefix(level), super.formatLog(level, message), Console.RESET)
  }

  override protected def write(level: LogLevel, message: String) = Console.err.println(message)

}





