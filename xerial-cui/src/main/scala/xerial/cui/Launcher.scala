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
// Launcher.scala
// Since: 2012/10/25 4:37 PM
//
//--------------------------------------

package xerial.cui

import xerial.core.util.{CName, CommandLineTokenizer}
import xerial.lens.{MethodCallBuilder, ObjectMethod, ObjectSchema}
import xerial.core.log.Logger

/**
 * Command launcher
 */
object Launcher extends Logger {

  def of[A <: AnyRef](implicit m:ClassManifest[A]) : Launcher = {
    new Launcher(m.erasure)
  }

  def execute[A <: AnyRef](argLine:String)(implicit m:ClassManifest[A]) : A = execute(CommandLineTokenizer.tokenize(argLine))(m)
  def execute[A <: AnyRef](args:Array[String])(implicit m:ClassManifest[A]) : A = {
    val l = Launcher.of[A]
    l.execute(args)
  }

  sealed trait Command {
    def execute[A <: AnyRef](mainObj:A, args:Array[String]) : A
  }


  class CommandDef(val method: ObjectMethod, val command: command) extends Command {
    val name = method.name
    val description = command.description
    def execute[A <: AnyRef](mainObj:A, args:Array[String]) : A = {
      trace("execute method: %s", name)
      val parser = new OptionParser(method)
      val r_sub = parser.parse(args)
      r_sub.build(new MethodCallBuilder(method, mainObj.asInstanceOf[AnyRef])).execute
      mainObj
    }
  }

  case class ModuleRef(moduleClass:Class[_], val name:String) extends Command {
    def execute[A <: AnyRef](mainObj:A, args:Array[String]) : A = {
      trace("execute module: %s", moduleClass)
      val result = new Launcher(moduleClass).execute[A](args)
      mainObj.asInstanceOf[Module].executedModule = Some((name, result.asInstanceOf[AnyRef]))
      mainObj
    }
  }
}

/**
 * Command launcher
 *
 * @author leo
 */
class Launcher(cl:Class[_]) extends Logger {
  import Launcher._

  private val schema = ClassOptionSchema(cl)

  def execute[A <: AnyRef](argLine:String) : A = execute(CommandLineTokenizer.tokenize(argLine))
  def execute[A <: AnyRef](args:Array[String]) : A = {
    val p = new OptionParser(schema)
    val r = p.parse(args, exitAfterFirstArgument=true)
    val mainObj : A = r.buildObjectWithFilter(cl, _ != "__commandName").asInstanceOf[A]
    val cn : Option[String] = (for((path, value) <- r.parseTree.dfs("__commandName")) yield value).toSeq.headOption
    cn.flatMap { commandName =>
      val c : Option[Command] = findCommand(commandName) orElse findModule(commandName, mainObj)
      c.map { _.execute(mainObj, r.unusedArgument) }
    } getOrElse mainObj
  }

  private lazy val commandList: Seq[CommandDef] = {
    trace("command class:" + cl.getName)
    ObjectSchema(cl).methods.flatMap {
      m => m.findAnnotationOf[command].map {
        x => new CommandDef(m, x)
      }
    }
  }


  private def findCommand(name: String): Option[Command] = {
    val cname = CName(name)
    trace("trying to find command:%s", cname)
    commandList.find(e => CName(e.name) == cname)
  }

  private def findModule[A <: AnyRef](name:String, mainObj:A) : Option[Command] = {
    if(Module.isModuleClass(mainObj.getClass))
      mainObj.asInstanceOf[Module].modules.get(name) map { cl => ModuleRef(cl, name) }
    else
      None
  }

}

