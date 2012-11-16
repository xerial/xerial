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

package xerial.lens.cui

import xerial.core.util.{CName, CommandLineTokenizer}
import xerial.lens.{TypeUtil, MethodCallBuilder, ObjectMethod, ObjectSchema}
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
    def name : String
    def description: String
    def printHelp : Unit
    def execute[A <: AnyRef](mainObj:A, args:Array[String], showHelp:Boolean) : A
  }


  private[Launcher] class CommandDef(val method: ObjectMethod, val command: command) extends Command {
    val name = method.name
    val description = command.description
    def printHelp = {
      val parser = new OptionParser(method)
      parser.printUsage
    }
    def execute[A <: AnyRef](mainObj:A, args:Array[String], showHelp:Boolean) : A = {
      trace("execute method: %s", name)
      val parser = new OptionParser(method)
      if(showHelp)
        parser.printUsage
      else {
        val r_sub = parser.parse(args)
        r_sub.build(new MethodCallBuilder(method, mainObj.asInstanceOf[AnyRef])).execute
      }
      mainObj
    }
  }

  private[Launcher] case class ModuleRef(m:ModuleDef) extends Command {
    def name = m.name
    def printHelp = {
      debug("module help")
      new Launcher(m.moduleClass).printHelp
    }
    def execute[A <: AnyRef](mainObj:A, args:Array[String], showHelp:Boolean) : A = {
      trace("execute module: %s", m.moduleClass)
      val result = new Launcher(m.moduleClass).execute[A](args, showHelp)
      mainObj.asInstanceOf[CommandModule].executedModule = Some((name, result.asInstanceOf[AnyRef]))
      mainObj
    }

    def description = m.description
  }

  private[cui] val commandNameParam = "command name"
}

/**
 * Implement this trait to supply a default command invoked when no command name is specified.
 */
trait DefaultCommand {
  def default : Unit
}

/**
 * Command launcher.
 *
 * {{{
 *
 * class MyCommand(@option(prefix="-h,--help", description="display help", isHelp=true) help:Boolean) {
 *
 *
 *   @command(description="Say hello")
 *   def hello(@option(prefix="-r", description="repeat times")
 *             repeat:Int=1,
 *             @argument
 *             message:String = "hello") {
 *      for(i <- 0 until repeat) println(message)
 *   }
 * }
 *
 * Launcher.execute[MyCommand]("hello -r 3")  // hello x 3
 *
 *
 *
 * }}}
 *
 *
 *
 *
 * @author leo
 */
class Launcher(cl:Class[_]) extends Logger {
  import Launcher._

  private val schema = ClassOptionSchema(cl)

  def execute[A <: AnyRef](argLine:String) : A = execute(CommandLineTokenizer.tokenize(argLine))
  def execute[A <: AnyRef](args:Array[String], showHelp:Boolean=false) : A = {
    val p = new OptionParser(schema)
    val r = p.parse(args)
    val mainObj : A = r.buildObjectWithFilter(cl, _ != commandNameParam).asInstanceOf[A]
    val cn : Option[String] = (for((path, value) <- r.parseTree.dfs(commandNameParam)) yield value).toSeq.headOption
    val helpIsOn = r.showHelp || showHelp
    val result = for(commandName <- cn; c <- findCommand(commandName, mainObj)) yield c.execute(mainObj, r.unusedArgument, helpIsOn)

    if(result.isEmpty) {
      if(helpIsOn)
        printHelp(p, mainObj)
      else if(classOf[DefaultCommand].isAssignableFrom(cl)) {
        // has a default command
        mainObj.asInstanceOf[DefaultCommand].default
      }
    }
    result getOrElse mainObj
  }

  def printHelp {
    printHelp(OptionParser(cl), TypeUtil.zero(cl).asInstanceOf[AnyRef])
  }

  def printHelp(p:OptionParser, obj:AnyRef) {
    trace("print usage")
    p.printUsage

    val lst = commandList ++ moduleList(obj)
    if(!lst.isEmpty) {
      println("[commands]")
      val maxCommandNameLen = lst.map(_.name.length).max
      val format = " %%-%ds\t%%s".format(math.max(10, maxCommandNameLen))
      lst.foreach {
        c =>
          println(format.format(c.name, c.description))
      }
    }
  }

  private lazy val commandList: Seq[Command] = {
    trace("command class:" + cl.getName)
    val lst = for(m <- ObjectSchema(cl).methods; c <- m.findAnnotationOf[command]) yield new CommandDef(m, c)
    lst
  }



  def moduleList[A <: AnyRef](mainObj:A) : Seq[Command] = {
    if(CommandModule.isModuleClass(mainObj.getClass))
      mainObj.asInstanceOf[CommandModule].modules.map( ModuleRef(_) )
    else
      Seq.empty
  }

  private def findCommand(name:String, mainObj:AnyRef) : Option[Command] = {

    def find(name: String): Option[Command] = {
    val cname = CName(name)
    trace("trying to find command:%s", cname)
      commandList.find(e => CName(e.name) == cname)
    }

    def findModule[A <: AnyRef](name:String, mainObj:A) : Option[Command] =
      moduleList(mainObj).find(_.name == name)


    find(name) orElse findModule(name, mainObj)
  }
}

