package xerial.core.cui

import xerial.core.XerialSpec
import xerial.core.log.LogLevel

//--------------------------------------
//
// CommandModuleTest.scala
// Since: 2012/08/30 9:13
//
//--------------------------------------

/**
 * @author Taro L. Saito
 */
class CommandModuleTest extends XerialSpec {

  class GlobalOption(@option(symbol = "h", description="display help message")
                     val displayHelp:Boolean=false,
                     @option(symbol="l", description="log level (trace|debug|info|warn|error|fatal)")
                     val logLevel : LogLevel=LogLevel.INFO)
  
  
  class MyModule(g:GlobalOption, 
                 @argument 
                 command:String) {
    
    @command
    def hello = "Hello World!"
    
    
  }
  
  class MyCommand(message:String, rep:Option[Int]) {

    def execute : String = {
      val r = rep getOrElse (1)
      val m = for(i <- 0 until r) yield
        "my command: " + message
      m.mkString("\n")
    }
  }
  
  
  "CommandModule" should {
    
    
    
  }
  
}