//--------------------------------------
//
// LoggerTest.scala
// Since: 2012/07/06 10:06 PM
//
//--------------------------------------

package xerial.core.util

import xerial.core.XerialSpec
import management.ManagementFactory
import javax.management.JMX
import javax.management.remote.{JMXServiceURL, JMXConnectorFactory}


/**
 * @author leo
 */
class LoggerTest extends XerialSpec {

  "Logger" should {
    "be used as an trait" in {
      true
    }

    "have root logger" in {
      val l = Logger.rootLogger
      l.log(LogLevel.INFO, "root logger")
    }


    "support nested logging" in {
      val l = logger('sub)
      l.debug("hello sub logger")
    }

    "support logging in recursion" in {
      debug("sub log test")
      def loop(i: Int): Unit = {
        if (i >= 0) {
          val l = logger('sub)
          l.debug("recursion: %d", i)
          loop(i - 1)
        }
      }
      loop(3)
    }


    "create helper method" in {
      val p = Seq("fatal", "error", "warn", "info", "debug", "trace").flatMap(l =>
        for (i <- 1 to 4) yield {
          val varList = (1 to i).map("a%d".format(_)).mkString(", ")
          val argList = (1 to i).map("a%d: => Any".format(_)).mkString(", ")
          val s = "def %s(format:String, %s) : Boolean = %s(format.format(%s))".format(l, argList, l, varList)
          s
        }
      )
      trace(p.mkString("\n"))
    }

    "Set log level via JMX" in {
      Logger.setLogLevelJMX("LoggerTest:jmx", "error")
      val l = Logger(classOf[LoggerTest], 'jmx)
      l.logLevel should be (LogLevel.ERROR)
    }

    "support file logger" in {
      val l = logger('file)


    }

  }

}