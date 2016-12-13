package xerial.core

import io.Resource
import xerial.core.util.Timer
import xerial.core.log.Logger
import org.scalatest._
import java.io.ByteArrayOutputStream
import scala.language.implicitConversions

//--------------------------------------
//
// XerialSpec.scala
// Since: 2012/07/06 4:09 PM
//
//--------------------------------------

/**
 * @author leo
 */
trait XerialSpec extends WordSpec with BeforeAndAfterAll with Matchers with GivenWhenThen with OptionValues with Resource with Timer with Logger {

  implicit def toTag(t:String) = Tag(t)

  /**
   * Captures the output stream and returns the printed messages as a String
   * @param body
   * @tparam U
   * @return
   */
  def captureOut[U](body: => U) : String = {
    val out = new ByteArrayOutputStream
    Console.withOut(out) {
      body
    }
    new String(out.toByteArray)
  }

  def captureErr[U](body: => U) : String = {
    val out = new ByteArrayOutputStream
    Console.withErr(out) {
      body
    }
    new String(out.toByteArray)
  }


}

