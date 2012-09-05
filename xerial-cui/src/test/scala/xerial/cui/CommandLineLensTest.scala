//--------------------------------------
//
// CommandLineLensTest.scala
// Since: 2012/07/06 4:08 PM
//
//--------------------------------------

package xerial.cui

import xerial.core.XerialSpec


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
  
  

}