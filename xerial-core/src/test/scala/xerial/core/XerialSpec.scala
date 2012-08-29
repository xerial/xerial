package xerial.core

import io.Resource
import org.scalatest.matchers.{MustMatchers, ShouldMatchers}
import util.StopWatch
import xerial.core.log.Logging
import org.scalatest.{Tag, OptionValues, GivenWhenThen, WordSpec}


//--------------------------------------
//
// XerialSpec.scala
// Since: 2012/07/06 4:09 PM
//
//--------------------------------------

/**
 * @author leo
 */
trait XerialSpec extends WordSpec with ShouldMatchers with MustMatchers with GivenWhenThen with OptionValues with Resource with Logging {

  implicit def toTag(t:String) = Tag(t)
}

