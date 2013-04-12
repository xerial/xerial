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
 * Timer trait measures the performance of code blocks.
 * Extend this trait and wrap the code to measure with `time(code_name){ ... }`:
 *
 * {{{
 * class A extends Timer {
 *   val t : TimeReport = time("performance test") {
 *     // write any code here
 *     block("A") {
 *       // code A
 *     }
 *     block("B") {
 *      // code B
 *     }
 *   }
 *   // report elapsed time of A, B and the total running time
 *   println(t)
 *
 *   t("A").average // the average of running time of code block "A" (min and max are also available)
 * }
 *
 * }}}
 *
 *
 * Timer can take the average of repetitive executions:
 *
 * {{{
 * class Rep extends Timer {
 *
 *   // Repeat 10 times the evaluation of the whole block
 *   val t = time("repetitive evaluation", repeat=10) {
 *      // This part will be executed 1000 x 10 times
 *      block("A", repeat=1000) {
 *        // code A
 *      }
 *
 *      // This part will be executed 1000 x 10 times
 *      block("B", repeat=1000) {
 *        // code B
 *      }
 *   }
 *
 *   println(t)
 *
 *   // Which code is faster?
 *   if(t("A") <= t("B"))
 *      println("A is faster")
 *   else
 *      println("B is faster")
 * }
 * }}}
 *
 * When measuring Scala (Java) code performances, you should take the average of execution times and reorder
 * the code block execution orders, because JVM has JIT compiler, which optimizes the code at runtime.
 * And also cache usage and the running state of the garbage core (GC) affects
 * the code performance. By repeating the executions of the entire or individual blocks with the `repeat` option,
 * you can avoid such pitfalls of benchmarking.
 *
 *
 *
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

  def min: Double =  minInterval
  def max: Double =  maxInterval

  def averageWithoutMinMax = {
    if(executionCount > 2)
      (s.getElapsedTime - min - max) / (_executionCount - 2)
    else
      average
  }

  def average: Double = {
    s.getElapsedTime / _executionCount
  }

  def elapsedSeconds: Double = {
    s.getElapsedTime
  }

  def toHumanReadableFormat(time:Double) : String = {
    val symbol = Seq("", "m", "n")

    val digits = math.log10(time)

    val unitIndex = {
      if(digits.isNaN || digits.isInfinity || digits >= -2.0)
        0
      else {
        val u = - ((digits - 1) / 3.0).toInt
        if(u >= symbol.length) symbol.length - 1 else u
      }
    }
    require(unitIndex >= 0 && (unitIndex < symbol.length), s"unitIndex must be between 0 to 2: $unitIndex, digits:$digits")
    val v = time * math.pow(10, unitIndex * 3)
    val str = f"$v%.3f ${symbol(unitIndex)}sec."
    f"$str%-11s"
  }

  def genReportLine: String = {
    f"-$name%-15s\ttotal:${toHumanReadableFormat(s.getElapsedTime)}, count:${executionCount}%,5d, avg:${toHumanReadableFormat(average)}, core avg:${toHumanReadableFormat(averageWithoutMinMax)}, min:${toHumanReadableFormat(minInterval)}, max:${toHumanReadableFormat(maxInterval)}"
  }

  def report: String = {
    def indent(level: Int, s: String): String = {
      (for (i <- 0 until level * 2) yield ' ').mkString + s
    }

    val lines = Seq.newBuilder[String]
    lines += indent(0, genReportLine)
    for ((k, v) <- subMeasure)
      lines += indent(1, v.genReportLine)

    lines.result.mkString("\n")
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


