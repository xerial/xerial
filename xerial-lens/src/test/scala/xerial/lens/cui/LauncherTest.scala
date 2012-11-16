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

package xerial.lens.cui

import xerial.core.XerialSpec
import xerial.core.log.{Logger, LogLevel}
import java.io.ByteArrayOutputStream

/**
 * @author leo
 */
class LauncherTest extends XerialSpec {

  import Launcher._

  "Launcher" should {

    import LauncherTest._


    /**
     * Captures the output stream and returns the printed messages as a String
     * @param body
     * @tparam U
     * @return
     */
    def capture[U](body: => U) : String = {
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

    "populate arguments in constructor" taggedAs("test1") in {
      capture {
        val l = Launcher.execute[GlobalOption]("-h -l debug")
        l.help should be(true)
        l.loglevel should be(Some(LogLevel.DEBUG))
        l.started should be(true)
      }
    }

    "populate arguments in constructor even when no parmeter is given" in {
      val l = Launcher.execute[GlobalOption]("")
      l.help should be (false)
      l.loglevel should be (None)
      l.started should be (true)
    }

    "display help message" in {
      val help = capture {
        val l = Launcher.execute[GlobalOption]("-h -l debug")
        l.started should be (true)
      }
      trace("help message:\n%s", help)
      help should (include ("-h"))
      help should (include ("--help"))
      help should (include ("-l"))
      help should (include ("--loglevel"))
    }

    "parse double hyphen options" in {
      capture {
        val l = Launcher.execute[GlobalOption]("--help --loglevel debug")
        l.help should be(true)
        l.loglevel should be(Some(LogLevel.DEBUG))
      }
    }

    "populate nested options" taggedAs("nested") in {
      capture {
        val l = Launcher.execute[NestedOption]("-h -l debug")
        l.g.help should be(true)
        l.g.loglevel should be(Some(LogLevel.DEBUG))
        l.g.started should be(true)
      }
    }

    "display help message of nested option" in {
      val help = capture {
        Launcher.execute[NestedOption]("-h -l debug")
      }
      help should (include ("-h"))
      help should (include ("--help"))
      help should (include ("-l"))
      help should (include ("--loglevel"))
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

    "display command list" in {
      val help = capture {
        Launcher.of[SimpleCommandSet].printHelp
      }
      trace("command list help:\n%s", help)
      help should (include("hello"))
      help should (include("say hello"))
      help should (include("world"))
      help should (include("say world"))
    }

    "run default command" in {
      val help = capture {
        Launcher.execute[SimpleCommandSet]("")
      }
      debug("default command message:\n%s", help)
      help should (include(DEFAULT_MESSAGE))
    }


    "create command modules" in {
      val c = Launcher.execute[MyCommandModule]("box hello")
      c.executedModule should be ('defined)
      c.executedModule map { m =>
        m._1 should be ("box")
        m._2.getClass should be (classOf[SimpleCommandSet])
        m._2.asInstanceOf[SimpleCommandSet].helloIsExecuted should be (true)
      }
      c.g should not be (null)
    }


    "display comand module help" in {
      val help = capture {
        Launcher.execute[MyCommandModule]("-h")
      }
      trace(help)
      help should (include("-h"))
      help should (include("-l"))
      help should (include("box"))
      help should (include("command set"))
    }

    "display individual command help" in {
      val help = capture {
        val l = Launcher.execute[MyCommandModule]("box --help")
        l.g.help should be (true)
      }
      trace(help)
      help should (include("hello"))
      help should (include("world"))
    }

    "display subcommand help" in {
      val help = capture {
        val l = Launcher.execute[MyCommandModule]("box world --help")
        l.g.help should be (true)
      }
      trace("box world --help:\n%s", help)
      help should (include("message"))
    }


    "handle private parameters in constructors" in {
      capture {
        val l = Launcher.execute[CommandWithPrivateField]("-h")
        l.started should be (true)
      }
    }

    "run test command" in {
      val message = capture {
        Launcher.execute[MyCommand]("hello -r 3")  // hello x 3
      }
      debug(message)
      message should (include ("hello!hello!hello!"))
    }

  }
}

object LauncherTest {

  case class GlobalOption(@option(prefix = "-h,--help", description = "display help messages", isHelp=true)
                          help: Boolean = false,
                          @option(prefix = "-l,--loglevel", description = "log level")
                          loglevel: Option[LogLevel] = None, var started: Boolean = false)
    extends Logger {

    trace("started GlobalOption command")
    started = true
  }


  class NestedOption(val g:GlobalOption) extends Logger {
    trace("started NestedOption command")

  }

  val DEFAULT_MESSAGE = "Type --help to display the list of commands"

  class SimpleCommandSet extends DefaultCommand with Logger {

    def default {
      println(DEFAULT_MESSAGE)
    }

    var helloIsExecuted = false
    @command(description = "say hello")
    def hello = {
      trace("hello")
      helloIsExecuted = true
    }
    @command(description = "say world")
    def world(@argument message:String) = debug("world world")
  }

  class MyCommandModule(val g:GlobalOption) extends CommandModule with Logger {
    def modules = Seq(ModuleDef("box", classOf[SimpleCommandSet], description="command set"))

    trace("global option: %s", g)
  }

  class CommandWithPrivateField
  (@option(prefix="-h,--help", description="display help", isHelp=true)
   help:Boolean, var started:Boolean = false) {
    started = true
  }


  class MyCommand(@option(prefix="-h,--help", description="display help", isHelp=true)
                  help:Boolean) {
    @command(description="say hello")
    def hello(@option(prefix="-r", description="repeat times")
              repeat:Int=1,
              @argument
              message:String = "hello!") {
      for(i <- 0 until repeat) print(message)
    }
  }

}