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
// LauncherTest.scala
// Since: 2012/10/25 4:38 PM
//
//--------------------------------------

package xerial.cui

import xerial.core.XerialSpec
import xerial.core.log.{Logger, LogLevel}

/**
 * @author leo
 */
class LauncherTest extends XerialSpec {

  import Launcher._

  "Launcher" should {

    import LauncherTest._

    "populate arguments in constructor" taggedAs("test1") in {
      val l = Launcher.execute[GlobalOption]("-h -l debug")
      l.help should be(true)
      l.loglevel should be(Some(LogLevel.DEBUG))
      l.started should be(true)
    }

    "parse double hyphen options" in {
      val l = Launcher.execute[GlobalOption]("--help --loglevel debug")
      l.help should be(true)
      l.loglevel should be(Some(LogLevel.DEBUG))
    }

    "populate nested options" taggedAs("nested") in {
      val l = Launcher.execute[NestedOption]("-h -l debug")
      l.g.help should be(true)
      l.g.loglevel should be(Some(LogLevel.DEBUG))
      l.g.started should be(true)
    }


  }
}

object LauncherTest {

  case class GlobalOption(@option(prefix = "-h,--help", description = "display help messages", isHelp=true)
                          val help: Boolean = false,
                          @option(prefix = "-l,--loglevel", description = "log level")
                          val loglevel: Option[LogLevel] = None, var started: Boolean = false)
    extends Logger {

    trace("started GlobalOption")
    started = true
  }


  class NestedOption(val g:GlobalOption) extends Logger {


  }

}