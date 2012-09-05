//--------------------------------------
//
// CommandLineLensTest.scala
// Since: 2012/07/06 4:08 PM
//
//--------------------------------------

package xerial.cui

import xerial.core.XerialSpec
import xerial.lens.ObjectSchema
import xerial.core.log.Logger
import xerial.core.util.Timer
import org.scalatest.Tag


object CommandLineLensTest {

  class GlobalOption(@option(symbol = "h", name = "help", description = "display help message")
                     displayHelp: Boolean = false,
                     @argument(index = 0, description = "command name")
                     commandName: Option[String])

  class CatOption(@option(symbol = "n", description = "show line number")
                  showLineNumber: Boolean = false)
  
  
  def cat(global:GlobalOption, option:CatOption) {

  }
}


/**
 * @author leo
 */
class CommandLineLensTest extends XerialSpec {
  
  "CommandLineLens" should {

    import ObjectSchema.toSchema


    "find annotations attached to method arguments" in {
      val methods = classOf[CommandLineAPI].methods
      val m = methods(0)
      m.name must be("hello")
      val name = m.findAnnotationOf[option](0).get
      name.symbol must be("n")
      name.description must be("name")
      val dh = m.findAnnotationOf[option](1).get
      dh.symbol must be("h")
      dh.description must be("display help message")
    }

    "find annotations of class parameters" in {
      val params = classOf[CommandLineOption].parameters
      val o1 = params(0)
      val a1 = o1.findAnnotationOf[option].get
      a1.symbol must be("h")
      val o2 = params(1)
      val a2 = o2.findAnnotationOf[option].get
      a2.symbol must be("f")

      val o3 = params(2)
      val a3 = o3.findAnnotationOf[option].get
      a3.symbol must be("o")
    }

    "create command module extending Timer" in {
      val l = CommandLauncher.of[MyModule]
      val r1 = l.execute("hello -s world")
      val r2 = l.execute("world")


      r1.get should be ("hello world!!!")
      r2.get should be ("hello world")
    }

    "compose commands from traits" taggedAs(Tag("compose-cmd")) in {
      val l = CommandLauncher.of[MyCmd]

      val r1 = l.execute("hello")
      val r2 = l.execute("helloWithArg world")

      r1.get should be ("hello my module2")
      r2.get should be ("hello world")
    }

  }

}


class CommandLineAPI {
  def hello(@option(symbol = "n", description = "name")
            name: String,
            @option(symbol = "h", description = "display help message")
            displayHelp: Option[Boolean]
             ): String = {
    "hello"
  }
}

class CommandLineOption
(
  @option(symbol = "h", description = "display help")
  val displayHelp: Option[Boolean],
  @option(symbol = "f", description = "input files")
  val files: Array[String]
  ) {
  @option(symbol = "o", description = "outdir")
  var outDir: String = "temp"
}

class MyModule extends CommandModule with Logger with Timer {

  @command(description = "say hello")
  def hello(@option(symbol="s") message:String) = {
    debug("runnign hello command")
    "hello %s!!!".format(message)
  }

  @command
  def world = {
    "hello world"
  }

  val moduleName = "mymodule"
}



trait MyModule2 {
  @command
  def hello = "hello my module2"

  @command
  def helloWithArg(@argument s:String) : String = "hello " + s
}

class MyCmd extends MyModule2 with CommandModule {
  val moduleName = "main"
}
