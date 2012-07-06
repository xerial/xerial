//--------------------------------------
//
// Logger.scala
// Since: 2012/07/06 4:18 PM
//
//--------------------------------------

package xerial.core.util

object LogLevel {

  object OFF extends LogLevel(0)
  object FATAL extends LogLevel(1)
  object ERROR extends LogLevel(2)
  object WARN extends LogLevel(3)
  object INFO extends LogLevel(4)
  object DEBUG extends LogLevel(5)
  object TRACE extends LogLevel(6)
  object ALL extends LogLevel(7)


}

sealed abstract class LogLevel(order:Int) extends Ordered[LogLevel] {
  def compare(other:LogLevel) = this.order - other.order
}

trait Logging {

  protected val _logger : Logger

  import LogLevel._


  protected def fatal(message: => Any): Boolean = log(FATAL, message)

  protected def error(message: => Any): Boolean = log(ERROR, message)

  protected def warn(message: => Any): Boolean = log(WARN, message)

  protected def info(message: => Any): Boolean = log(INFO, message)

  protected def debug(message: => Any): Boolean = log(DEBUG, message)

  protected def trace(message: => Any): Boolean = log(TRACE, message)

  protected def log(logLevel: LogLevel, message: => Any): Boolean = {
    if(_logger.isEnabled(logLevel)) {
      _logger.log(logLevel, message)
      true
    }
    else
      false
  }


}

object Logger {

  def apply(cl:Class[_]) : Logger = {



  }

}


/**
 * @author leo
 */
trait Logger {

  private val loggerName = this.getClass.getName
  private val logLevel : LogLevel

  def setLogLevel(logLevel:LogLevel)

  def isEnabled(targetLogLevel:LogLevel) : Boolean = targetLogLevel <= logLevel

  def log(level:LogLevel, message: => Any) : Boolean

}


class StandardLogger(name:String, val ) extends Logger {
  private val logLevel = _

  def setLogLevel(logLevel: LogLevel) {}

  def log(level: LogLevel, message: => Any) = false
}







