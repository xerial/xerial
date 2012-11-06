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

//--------------------------------------
//
// LogWriter.scalaala
// Since: 2012/07/19 4:23 PM
//
//--------------------------------------

package xerial.core.log

import collection.mutable
import javax.management.{MBeanServerConnection, JMX, ObjectName}
import management.ManagementFactory
import javax.management.remote.{JMXConnectorFactory, JMXServiceURL}
import util.DynamicVariable

/**
 * log level definitions
 *
 * @author Taro L. Saito
 */
object LogLevel extends xerial.core.collection.Enum[LogLevel] {

  object OFF extends LogLevel(0, "off")
  object FATAL extends LogLevel(1, "fatal")
  object ERROR extends LogLevel(2, "error")
  object WARN extends LogLevel(3, "warn")
  object INFO extends LogLevel(4, "info")
  object DEBUG extends LogLevel(5, "debug")
  object TRACE extends LogLevel(6, "trace")
  object ALL extends LogLevel(7, "all")

  val values = IndexedSeq(OFF, FATAL, ERROR, WARN, INFO, DEBUG, TRACE, ALL)
  private lazy val index = values.map { l => l.name.toLowerCase -> l } toMap

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

  def unapply(name:String) : Option[LogLevel] = index.get(name.toLowerCase)
}

sealed abstract class LogLevel(val order: Int, val name: String) extends Ordered[LogLevel] {
  def compare(other: LogLevel) = this.order - other.order
  override def toString = name
}

/**
 * Trait for adding logging functions (trace, debug, info, warn, error and fatal) to your class.
 */
trait Logger {

  private[this] val logger = new DynamicVariable[LogWriter](LoggerFactory(this.getClass))

  def log(logLevel: LogLevel, message: => Any): Unit = {
    if (logger.value.isEnabled(logLevel))
      logger.value.log(logLevel, message)
  }

  protected def withLogger[U](tag:String)(body: => U) : U = {
    if(logger.value.tag == tag)
      body
    else
      logger.withValue(LoggerFactory(logger.value.prefix + ":" + tag)) {
        body
      }
  }

  /**
   * Create a sub logger with a given tag name
   * @param tag
   * @return
   */
  protected def getLogger(tag: Symbol): LogWriter = LoggerFactory(logger.value, tag)

  /**
   * Create a sub logger with a given tag name
   * @param tag
   * @return
   */
  protected def getLogger(tag: String): LogWriter = getLogger(Symbol(tag))

  protected def log[U](tag: String)(f: LogWriter => U) {
    f(getLogger(tag))
  }

  protected def fatal(message: => Any): Unit = log(LogLevel.FATAL, message)
  protected def error(message: => Any): Unit = log(LogLevel.ERROR, message)
  protected def warn(message: => Any): Unit = log(LogLevel.WARN, message)
  protected def info(message: => Any): Unit = log(LogLevel.INFO, message)
  protected def debug(message: => Any): Unit = log(LogLevel.DEBUG, message)
  protected def trace(message: => Any): Unit = log(LogLevel.TRACE, message)

  protected def fatal(format: String, a1: => Any): Unit = fatal(format.format(a1))
  protected def fatal(format: String, a1: => Any, a2: => Any): Unit = fatal(format.format(a1, a2))
  protected def fatal(format: String, a1: => Any, a2: => Any, a3: => Any): Unit = fatal(format.format(a1, a2, a3))
  protected def fatal(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Unit = fatal(format.format(a1, a2, a3, a4))
  protected def error(format: String, a1: => Any): Unit = error(format.format(a1))
  protected def error(format: String, a1: => Any, a2: => Any): Unit = error(format.format(a1, a2))
  protected def error(format: String, a1: => Any, a2: => Any, a3: => Any): Unit = error(format.format(a1, a2, a3))
  protected def error(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Unit = error(format.format(a1, a2, a3, a4))
  protected def warn(format: String, a1: => Any): Unit = warn(format.format(a1))
  protected def warn(format: String, a1: => Any, a2: => Any): Unit = warn(format.format(a1, a2))
  protected def warn(format: String, a1: => Any, a2: => Any, a3: => Any): Unit = warn(format.format(a1, a2, a3))
  protected def warn(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Unit = warn(format.format(a1, a2, a3, a4))
  protected def info(format: String, a1: => Any): Unit = info(format.format(a1))
  protected def info(format: String, a1: => Any, a2: => Any): Unit = info(format.format(a1, a2))
  protected def info(format: String, a1: => Any, a2: => Any, a3: => Any): Unit = info(format.format(a1, a2, a3))
  protected def info(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Unit = info(format.format(a1, a2, a3, a4))
  protected def debug(format: String, a1: => Any): Unit = debug(format.format(a1))
  protected def debug(format: String, a1: => Any, a2: => Any): Unit = debug(format.format(a1, a2))
  protected def debug(format: String, a1: => Any, a2: => Any, a3: => Any): Unit = debug(format.format(a1, a2, a3))
  protected def debug(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Unit = debug(format.format(a1, a2, a3, a4))
  protected def trace(format: String, a1: => Any): Unit = trace(format.format(a1))
  protected def trace(format: String, a1: => Any, a2: => Any): Unit = trace(format.format(a1, a2))
  protected def trace(format: String, a1: => Any, a2: => Any, a3: => Any): Unit = trace(format.format(a1, a2, a3))
  protected def trace(format: String, a1: => Any, a2: => Any, a3: => Any, a4: => Any): Unit = trace(format.format(a1, a2, a3, a4))


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
 * Interface of log writers
 * @author leo
 */
trait LogWriter extends LogHelper {

  val name: String
  val shortName = LoggerFactory.leafName(name)
  def tag = {
    val pos = shortName.lastIndexOf(":")
    if (pos == -1)
      ""
    else
      shortName.substring(pos + 1)
  }
  def prefix : String = {
    val pos = name.lastIndexOf(":")
    if(pos == -1) name else name.substring(0, pos)
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


object LoggerFactory {

  private[log] var defaultLogLevel: LogLevel = LogLevel(System.getProperty("loglevel", "info"))

  private val rootLoggerName = "root"
  val rootLogger = new ConsoleLogWriter(rootLoggerName, defaultLogLevel)

  /**
   * Hold logger instances in weakly referenced hash map to allow releasing instances when necessary
   */
  private val loggerHolder = new mutable.WeakHashMap[String, LogWriter]


  def apply(cl: Class[_]): LogWriter = getLogger(cl.getName)
  def apply(name: String): LogWriter = getLogger(name)
  def apply(logger: LogWriter, symbol: Symbol): LogWriter = getLogger(logger.name + ":" + symbol.name)
  def apply(cl: Class[_], symbol: Symbol): LogWriter = apply(apply(cl), symbol)


  def getDefaultLogLevel(loggerName:String) : LogLevel ={
    def property(key:String) : Option[String] = Option(System.getProperty(key))
    def parents : Seq[String] = {
      val c = loggerName.split(".")
      val p = for(i <- 1 until c.length) yield {
        c.take(i).mkString(".")
      }
      p.reverse
    } 

    val l = Seq(loggerName, leafName(loggerName)) ++ parents
    val ll : Option[String] = (for(k <- l.map("loglevel:%s".format(_)); prop <- property(k)) yield prop).headOption
    ll.map(LogLevel(_)).getOrElse(defaultLogLevel)
  }

  /**
   * Get the logger of the specified name. LogWriter names are
   * dot-separated list as of the package names.
   * This naming should be the same with java package/class naming convention.
   */
  private def getLogger(name: String): LogWriter = {

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
        loggerHolder.getOrElseUpdate(name, new ConsoleLogWriter(name, getLogLevel))
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
    val addr = getJMXServerAddress(pid)
    val server : Option[MBeanServerConnection] = addr.map{ a =>
      JMXConnectorFactory.connect(new JMXServiceURL(a))
    } map (_.getMBeanServerConnection)

    if(server.isEmpty)
      rootLogger.warn("No JMX server (pid:%d) is found", pid)
    else {
      rootLogger.info("Found a JMX server", pid)
      rootLogger.debug("Server address: %s", addr.get)
    }
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

}

import javax.management.MXBean
/**
 * LogWriter configuration API
 * @author leo
 */
@MXBean abstract trait LoggerConfig {
  def setLogLevel(name: String, logLevel: String): Unit
  def setDefaultLogLevel(logLevel:String) : Unit
  def getDefaultLogLevel : String
}


class LoggerConfigImpl extends LoggerConfig {

  def getDefaultLogLevel = LoggerFactory.defaultLogLevel.toString

  def setLogLevel(loggerName: String, logLevel: String)  {
    System.setProperty("loglevel:%s".format(loggerName), logLevel)
    val logger = LoggerFactory.apply(loggerName)
    val level = LogLevel(logLevel)
    logger.logLevel = level
    LoggerFactory.rootLogger.info("set the log level of %s to %s", loggerName, level)
  }

  def setDefaultLogLevel(logLevel:String) {
    val level = LogLevel(logLevel)
    System.setProperty("loglevel", level.toString)
    LoggerFactory.rootLogger.info("Set the default log level to %s", level)
  }
}



/**
 * Empty log writer
 */
trait NullLogWriter extends LogWriter {
  protected def write(level: LogLevel, message: Any) {}
}

/**
 * LogWriter for string messages
 */
trait StringLogWriter extends LogWriter {
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

object ConsoleLogWriter {

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

/**
 * Log writer using console
 * @param name
 * @param logLevel
 */
class ConsoleLogWriter(val name: String, var logLevel: LogLevel) extends StringLogWriter {

  override protected def formatLog(level:LogLevel, message: Any) = {
    "%s%s%s".format(ConsoleLogWriter.colorPrefix(level), super.formatLog(level, message), Console.RESET)
  }

  override protected def write(level: LogLevel, message: String) = Console.err.println(message)

}





