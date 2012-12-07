/*
 * Copyright 2012 Taro L. Saito
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//--------------------------------------
//
// DataUnitTest.scala
// Since: 2012/12/07 1:51 PM
//
//--------------------------------------

package xerial.core.util

import xerial.core.XerialSpec

/**
 * @author Taro L. Saito
 */
class DataUnitTest extends XerialSpec {
  "DataUnit" should {
    "translate data size" in {
      var unit = 1L

      DataUnit.toHumanReadableFormat(unit) should be ("1")
      unit *= 1024L
      DataUnit.toHumanReadableFormat(unit) should be ("1K")
      unit *= 1024L
      DataUnit.toHumanReadableFormat(unit) should be ("1M")
      unit *= 1024L
      DataUnit.toHumanReadableFormat(unit) should be ("1G")
      unit *= 1024L
      DataUnit.toHumanReadableFormat(unit) should be ("1T")
      unit *= 1024L
      DataUnit.toHumanReadableFormat(unit) should be ("1P")
      unit *= 1024L
      DataUnit.toHumanReadableFormat(unit) should be ("1E")


    }
  }
}