//--------------------------------------
//
// LoggerTest.scala
// Since: 2012/07/06 10:06 PM
//
//--------------------------------------

package xerial.core.log

import xerial.core.XerialSpec


/**
 * @author leo
 */
class LoggerTest extends XerialSpec {

  "LogWriter" should {
    "be used as an trait" in {
      true
    }

    "have root logger" in {
      val l = LoggerFactory.rootLogger
      l.log(LogLevel.INFO, "root logger")
    }


    "support nested logging" in {
      val l = getLogger('sub)
      l.debug("hello sub logger")
    }

    "support logging in recursion" in {
      debug("sub log test")
      def loop(i: Int): Unit = {
        if (i >= 0) {
          withLogger("sub") {
            debug("recursion: %d", i)
            loop(i - 1)
          }
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
      LoggerFactory.setLogLevelJMX("LoggerTest:jmx", "error")
      val l = LoggerFactory(classOf[LoggerTest], 'jmx)
      l.logLevel should be(LogLevel.ERROR)
    }

    "support file logger" in {
      pending
      val l = getLogger('file)
    }

  }

}