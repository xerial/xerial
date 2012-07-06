package xerial.core

import org.scalatest.fixture.WordSpec
import org.scalatest.matchers.{MustMatchers, ShouldMatchers}
import org.scalatest.{OptionValues, GivenWhenThen}

//--------------------------------------
//
// XerialSpec.scala
// Since: 2012/07/06 4:09 PM
//
//--------------------------------------

/**
 * @author leo
 */
trait XerialSpec extends WordSpec with ShouldMatchers with MustMatchers with GivenWhenThen with OptionValues

