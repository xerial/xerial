package xerial.core

import org.scalatest.WordSpec
import org.scalatest.matchers.{MustMatchers, ShouldMatchers}
import org.scalatest._
import xerial.core.log.Logging


//--------------------------------------
//
// XerialSpec.scala
// Since: 2012/07/06 4:09 PM
//
//--------------------------------------

/**
 * @author leo
 */
trait XerialSpec extends WordSpec with ShouldMatchers with MustMatchers with GivenWhenThen with OptionValues with Logging

