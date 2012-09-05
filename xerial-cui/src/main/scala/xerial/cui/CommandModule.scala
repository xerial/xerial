/*
 * Copyright 2012 Taro L. Saito
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package xerial.cui

import java.lang.reflect.InvocationTargetException
import xerial.cui._
import xerial.core.log.Logger
import xerial.core.util.{CommandLineTokenizer, CName}
import xerial.lens.{ObjectMethod, MethodCallBuilder, ObjectSchema}


//--------------------------------------
//
// CommandLauncher.scala
// Since: 2012/03/22 14:28
//
//--------------------------------------

object CommandLauncher {

  def of[A <: CommandModule](implicit m: ClassManifest[A]) = {
    new CommandLauncher[A](m.erasure.asInstanceOf[Class[A]])
  }

}

/**
 *
 */
trait CommandModule extends Logger {
  self =>
  type A = self.type

  val moduleName: String

  @option(symbol = "h", name="help", description = "display help message")
  var displayHelp: Boolean = false

  @argument(index = 0, name = "command name", description = "sub command name")
  var commandName: Option[String] = None

  /**
   * Called before calling execute. If return value is true, continue the execution. If false, do not call execute command.
   * Override this method if you want to add custom execution phase. Global options defined in modules are already set.
   * @param args
   * @return
   */
  def beforeExecute(args: Array[String]): Boolean = true

  def execute(unusedArgs: Array[String]): Any = {

    trace("display help:%s, unused args:%s", displayHelp, unusedArgs.mkString(", "))
    if (displayHelp) {
      trace("command name:%s", commandName)
      commandName match {
        case Some(cmd) => printUsage(cmd)
        case None => printUsage
      }
    }
    else {
      trace("command name:%s", commandName)
      commandName match {
        case Some(n) => {
          val command = findCommand(n)
          trace("run command:%s, commandName:%s", command, commandName)
          val result = command.map {
            c =>
              val parser = new OptionParser(c.method)
              val builder = new MethodCallBuilder(c.method, this)
              parser.build(unusedArgs, builder)

              try
                builder.execute
              catch {
                case e: InvocationTargetException => throw e.getTargetException
              }
          }
          if (result.isDefined) result.get else null
        }
        case None => printProgName
      }
    }

  }

  def printProgName {
    println("Type --help to see the list of commands")
  }

  private lazy val commandList: Seq[CommandDef] = {
    val cl = this.getClass
    trace("command class:" + cl.getName)

    ObjectSchema(this.getClass).methods.flatMap {
      m => m.findAnnotationOf[command].map {
        x => new CommandDef(m, x)
      }
    }
  }


  private def findCommand(name: String): Option[CommandDef] = {
    val cname = CName(name)
    trace("find command:%s", cname)
    commandList.find(e => CName(e.name) == cname)
  }

  def printUsage {
    trace("print usage")
    OptionParser(this.getClass).printUsage

    println("[commands]")
    val maxCommandNameLen = commandList.map(_.name.length).max
    val format = " %%-%ds\t%%s".format(math.max(10, maxCommandNameLen))
    commandList.foreach {
      c =>
        println(format.format(c.name, c.description))
    }

  }

  def printUsage(commandName: String) {
    trace("print usage of %s", commandName)
    findCommand(commandName).map {
      c =>
        val parser = new OptionParser(c.method)
        parser.printUsage
    }
  }


}

class CommandDef(val method: ObjectMethod, val command: command) {
  val name = method.name
  val description = command.description
}


/**
 * CommandTrait-launcher
 *
 * @author leo
 */
class CommandLauncher[A <: CommandModule](cl:Class[A])(implicit m:ClassManifest[A]) extends Logger {

  trace("launcher class: %s", cl.getName)

  def execute(argLine: String): Option[_] =
    execute(CommandLineTokenizer.tokenize(argLine))

  def execute(args: Array[String]): Option[_] = {

    val parser = OptionParser.of[A]
    val (module, parseResult) = parser.build[A](args)

    if (module.beforeExecute(args)) {
      Some(module.execute(parseResult.unusedArgument))
    }
    else
      None
  }

}
