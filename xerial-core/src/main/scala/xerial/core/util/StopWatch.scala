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

package xerial.core.util

import java.lang.IllegalStateException
import collection.mutable.{ListBuffer, Stack, LinkedHashMap}
import xerial.core.log.{LoggerFactory, Logger, LogLevel}


//--------------------------------------
//
// StopWatch.scala
// Since: 2012/01/09 8:31
//
//--------------------------------------

/**
 * Timer trait for measuring the performance of code blocks
 *
 * <code>
 * <pre>
 *
 *   class A extends Timer {
 *
 *     val t : TimeReport = time("performance test") {
 *       // write any code here
 *       block("A") {
 *        // code A
 *       }
 *       block("B") {
 *        // code B
 *       }
 *     }
 *
 *     // report elapsed time of A, B and the total time
 *     println(t)
 *
 *   }
 *
 * </pre>
 * </code>
 * @author leo
 */
trait Timer {
  private[this] val holder = new ThreadLocal[Stack[TimeReport]] {
    override def initialValue() = new Stack[TimeReport]
  }

  private def contextStack = holder.get()

  private def createNewBlock[A](blockName: String, f: => A): TimeReport = new TimeReport {
    val name: String = blockName
    def body() = f
  }

  import LogLevel._

  /**
   * Measure the execution time of the code block
   * @param blockName
   * @param logLevel
   * @param repeat the number of repetitive execution
   * @param f
   * @tparam A
   * @return
   */
  protected def time[A](blockName: String, logLevel: LogLevel = DEBUG, repeat: Int = 1)(f: => A): TimeReport = {
    def pushContext(t: TimeReport): Unit = contextStack.push(t)
    def popContext: Unit = contextStack.pop

    val m = createNewBlock(blockName, f)
    try {
      pushContext(m)
      m.measure(repeat)
    }
    finally {
      popContext
      reportLog(m, logLevel)
    }
  }

  protected def block[A](name: String, repeat: Int = 1)(f: => A): TimeReport = {
    val m = contextStack.lastOption match {
      case None => throw new IllegalStateException("block {} should be enclosed inside time {}")
      case Some(context) => {
        context.getOrElseUpdate(name, createNewBlock(name, f))
      }
    }
    m.measure(repeat)
  }

  protected def reportLog(m: TimeReport, logLevel: LogLevel): Unit = {
    val l = if (classOf[Logger].isAssignableFrom(this.getClass))
      this.asInstanceOf[Logger].log(logLevel, m.report)
    else
      LoggerFactory(this.getClass).log(logLevel, m.report)
  }
}

/**
 * Summary of the execution times of the code blocks
 */
trait TimeReport extends Ordered[TimeReport] {
  val name: String
  def body(): Unit

  private[TimeReport] val s = new StopWatch
  private lazy val subMeasure = new LinkedHashMap[String, TimeReport]
  private var _executionCount = 0

  private var maxInterval: Double = 0.0
  private var minInterval: Double = Double.MaxValue

  {
    s.stop
    s.reset
  }

  def containsBlock(name: String) = {
    subMeasure.contains(name)
  }

  def apply(name: String): TimeReport = {
    subMeasure(name)
  }

  def getOrElseUpdate(name: String, t: => TimeReport): TimeReport = {
    subMeasure.getOrElseUpdate(name, t)
  }

  def executionCount: Int = _executionCount

  def measure(repeat: Int = 1): TimeReport = {
    for (i <- 0 until repeat) {
      s.resume
      try {
        body
      }
      finally {
        val intervalTime = s.stop
        _executionCount += 1

        maxInterval = math.max(maxInterval, intervalTime)
        minInterval = math.min(minInterval, intervalTime)
      }
    }
    this
  }


  def compare(that: TimeReport) =
    this.elapsedSeconds.compareTo(that.elapsedSeconds)


  def average: Double = {
    s.getElapsedTime / _executionCount
  }

  def elapsedSeconds: Double = {
    s.getElapsedTime
  }

  def genReportLine: String = {
    "-%-10s\ttotal:%.3f sec., count:%,5d, avg:%.3f sec., min:%.3f sec., max:%.3f sec.".format(
      name, s.getElapsedTime, executionCount, average, minInterval, maxInterval
    )
  }

  def report: String = {
    def indent(level: Int, s: String): String = {
      (for (i <- 0 until level * 2) yield ' ').mkString + s
    }

    val lines = new ListBuffer[String]
    lines += indent(0, genReportLine)
    for ((k, v) <- subMeasure)
      lines += indent(1, v.genReportLine)

    lines.mkString("\n")
  }

  override def toString: String = report

}


class StopWatch {

  private[StopWatch] object State extends Enumeration {
    val RUNNING, STOPPED = Value
  }

  private var lastSystemTime: Double = System.nanoTime
  private var elapsedTimeAccumulated: Double = 0L
  private var state = State.RUNNING

  private val NANO_UNIT: Double = 1000000000L

  /**
   * Gets the elapsed time since this instance is created in seconds.
   *
   * @return the elapsed time in seconds.
   */
  def getElapsedTime: Double = {

    if (state == State.RUNNING) {
      val now = System.nanoTime()
      val diff = now - lastSystemTime
      (elapsedTimeAccumulated + diff) / NANO_UNIT
    }
    else {
      elapsedTimeAccumulated / NANO_UNIT
    }
  }

  /**
   * Reset the stop watch. The subsequent calls to
   * getElapsedTime or getIntervalTiem will measure the time interval
   * beginning from this method call.
   */
  def reset = {
    lastSystemTime = System.nanoTime()
    elapsedTimeAccumulated = 0L
  }

  /**
   * Stop the timer
   * @return interval time since the last resume call
   */
  def stop: Double = {
    if (state == State.STOPPED)
      return 0.0

    // elapsed time
    val now = System.nanoTime()
    val diff = now - lastSystemTime
    elapsedTimeAccumulated += diff
    lastSystemTime = now;

    state = State.STOPPED;
    diff / NANO_UNIT
  }

  /**
   * Resume the timer
   */
  def resume {
    if (state == State.RUNNING)
      return

    lastSystemTime = System.nanoTime();
    state = State.RUNNING;
  }

  def reportElapsedTime: String = {
    "%.2f sec." format (getElapsedTime)
  }


}


