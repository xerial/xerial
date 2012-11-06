//--------------------------------------
//
// CommandLineLensTest.scala
// Since: 2012/07/06 4:08 PM
//
//--------------------------------------

package xerial.cui

import xerial.core.XerialSpec
import xerial.lens.ObjectSchema
import xerial.core.util.Timer
import org.scalatest.Tag
import xerial.core.log.{LogLevel, Logger}



object CommandLineLensTest {

  class GlobalOption(@option(prefix="-h,--help", description = "display help message")
                     displayHelp: Boolean = false,
                     @argument(index = 0, description = "command name")
                     commandName: Option[String])

  class CatOption(@option(prefix = "-n", description = "show line number")
                  showLineNumber: Boolean = false)
  
  
  def cat(global:GlobalOption, option:CatOption) {

  }



  class MyCommand(displayHelp:Boolean = false, logLevel:LogLevel = LogLevel.INFO) {

    def hello(message:String) {

      
    }

  }

  class CommandLineAPI {
    def hello(@option(prefix = "-n", description = "name")
              name: String,
              @option(prefix = "-h", description = "display help message")
              displayHelp: Option[Boolean]
               ): String = {
      "hello"
    }
  }

  class CommandLineOption
  (
    @option(prefix = "-h", description = "display help")
    val displayHelp: Option[Boolean],
    @option(prefix = "-f", description = "input files")
    val files: Array[String]
    ) {
    @option(prefix = "-o", description = "outdir")
    var outDir: String = "temp"
  }

}


/**
 * @author leo
 */
class CommandLineLensTest extends XerialSpec {
  
  "CommandLineLens" should {

    import ObjectSchema.toSchema

    import CommandLineLensTest._

    "find annotations attached to method arguments" in {
      val methods = classOf[CommandLineAPI].methods
      val m = methods(0)
      m.name must be("hello")
      val name = m.findAnnotationOf[option](0).get
      name.prefix must be("-n")
      name.description must be("name")
      val dh = m.findAnnotationOf[option](1).get
      dh.prefix must be("-h")
      dh.description must be("display help message")
    }

    "find annotations of class parameters" in {
      val params = classOf[CommandLineOption].parameters
      val o1 = params(0)
      val a1 = o1.findAnnotationOf[option].get
      a1.prefix must be("-h")
      val o2 = params(1)
      val a2 = o2.findAnnotationOf[option].get
      a2.prefix must be("-f")

      val o3 = params(2)
      val a3 = o3.findAnnotationOf[option].get
      a3.prefix must be("-o")
    }

  }

}

