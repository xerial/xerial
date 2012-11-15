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

    "populate arguments in constructor even when no parmeter is given" in {
      val l = Launcher.execute[GlobalOption]("")
      l.help should be (false)
      l.loglevel should be (None)
      l.started should be (true)
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

    "populate nested options even when no paramter is given"  taggedAs("nested2") in {
      val l = Launcher.execute[NestedOption]("")
      l.g should not be (null)
      l.g.help should be(false)
      l.g.loglevel should be(None)
      l.g.started should be(true)
    }

    "find commands" in {
      val c = Launcher.execute[SimpleCommandSet]("hello")
      c.helloIsExecuted should be (true)
    }

    "create command modules" in {
      val c = Launcher.execute[CommandModule]("box hello")
      c.executedModule should be ('defined)
      c.executedModule map { m =>
        m._1 should be ("box")
        m._2.getClass should be (classOf[SimpleCommandSet])
        m._2.asInstanceOf[SimpleCommandSet].helloIsExecuted should be (true)
      }
      c.g should not be (null)
    }

  }
}

object LauncherTest {

  case class GlobalOption(@option(prefix = "-h,--help", description = "display help messages", isHelp=true)
                          val help: Boolean = false,
                          @option(prefix = "-l,--loglevel", description = "log level")
                          val loglevel: Option[LogLevel] = None, var started: Boolean = false)
    extends Logger {

    debug("started GlobalOption command")
    started = true
  }


  class NestedOption(val g:GlobalOption) extends Logger {
    debug("started NestedOption command")

  }

  class SimpleCommandSet extends Logger {
    var helloIsExecuted = false
    @command
    def hello = {
      debug("hello")
      helloIsExecuted = true
    }
    @command
    def hello2(message:String) = debug("hello hello")
  }

  class CommandModule(val g:GlobalOption) extends Module with Logger {
    def modules = Map("box" -> classOf[SimpleCommandSet])

    debug("global option: %s", g)
  }

}