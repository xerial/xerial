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

package xerial.core.util

import collection.mutable.WeakHashMap
import java.io.File
import sys.process.Process
import xerial.core.log.Logger
import xerial.core.util
import java.lang.reflect.Field

//--------------------------------------
//
// Shell.scala
// Since: 2012/02/06 10:02
//
//--------------------------------------




/**
 * Launch UNIX (or cygwin) commands from Scala
 * @author leo
 */
object Shell extends Logger {

  private def access[U](f:Field)(body: => U) : U = {
    val a = f.isAccessible
    try {
      if(!a)
        f.setAccessible(true)
      body
    }
    finally {
      if(!a)
        f.setAccessible(a)
    }

  }

  /**
   * Returns process id
   * @param p
   * @return process id or -1 if pid cannot be detected
   */
  def getProcessID(p:java.lang.Process) : Int = {
    try {
      // If the current OS is *Nix, the process class of p is UNIXProcess and its pid can be obtained
      // from pid field.
      val f = p.getClass().getDeclaredField("pid")
      val pid : Int = access(f) {
        f.get(p).asInstanceOf[Int]
      }
      pid
    }
    catch {
      case e => -1
    }
  }

  def launchJava(args: String) = {
    val javaCmd = Shell.findJavaCommand()
    if (javaCmd.isEmpty)
      throw new IllegalStateException("No JVM is found. Set JAVA_HOME environmental variable")

    val cmdLine = "%s %s".format(javaCmd.get, args)
    launchProcess(cmdLine)
  }


  def launchProcess(cmdLine: String) = {
    val c = "%s -c \"%s\"".format(Shell.getCommand("sh"), cmdLine)
    val pb = new ProcessBuilder(CommandLineTokenizer.tokenize(c):_*)
    pb.inheritIO()
    var env = getEnv
    if(OS.isWindows)
      env += ("CYGWIN" -> "notty")
    val envMap = pb.environment()
    env.foreach(e => envMap.put(e._1, e._2))
    val p = pb.start()
    debug("exec command [pid:%d] %s", getProcessID(p), c)
    p
  }

  def getEnv : Map[String, String] = {
    import collection.JavaConversions._
    System.getenv().toMap
  }

  def launchCmdExe(cmdLine: String) = {
    val c = "%s /c \"%s\"".format(Shell.getCommand("cmd"), cmdLine)
    debug {
      "exec command: " + c
    }

    Process(CommandLineTokenizer.tokenize(c), None, getEnv.toSeq:_*).run
  }


  // command name -> path
  private val cmdPathCache = new WeakHashMap[String, Option[String]]

  def getCommand(name: String): String = {

    findCommand(name) match {
      case Some(cmd) => cmd
      case None => throw new IllegalStateException("CommandTrait not found: %s".format(name))
    }
  }

  def findSh: Option[String] = {
    findCommand("sh")
  }

  def getExecPath = {
    val path = (System.getenv("PATH") match {
      case null => ""
      case x => x.toString
    }).split("[;:]")
    path
  }

  /**
   * Return OS-dependent program name. (e.g., sh in Unix, sh.exe in Windows)
   */
  def progName(p: String) = {
    if (OS.isWindows)
      p + ".exe"
    else
      p
  }


  def findCommand(name: String): Option[String] = {
    cmdPathCache.getOrElseUpdate(name, {
      val path = {
        if (OS.isWindows)
          getExecPath ++ Seq("c:/cygwin/bin")
        else
          getExecPath
      }
      val prog = progName(name)

      val exe = path.map(new File(_, prog)).find(_.exists).map(_.getAbsolutePath)
      trace {
        if (exe.isDefined) "%s is found at %s".format(name, exe.get)
        else "%s is not found".format(name)
      }
      exe
    })
  }

  def sysProp(key:String) : Option[String] = Option(System.getProperty(key))
  def env(key:String) : Option[String] = Option(System.getenv(key))


  def findJavaHome: Option[String] = {

    // lookup environment variable JAVA_HOME first.
    // If JAVA_HOME is not defined, use java.home system property
    val e : Option[String] = env("JAVA_HOME") orElse sysProp("java.home")

    def resolveCygpath(p: String) = {
      if (OS.isWindows) {
        // If the path is for Cygwin environment
        val m = """/cygdrive/(\w)(/.*)""".r.findFirstMatchIn(p)
        if (m.isDefined)
          "%s:%s".format(m.get.group(1), m.get.group(2))
        else
          p
      }
      else
        p
    }

    val p = e.map(resolveCygpath(_))
    debug("Found JAVA_HOME=" + p.get)
    p
  }


  def findJavaCommand(javaCmdName: String = "java"): Option[String] = {

    def search: Option[String] = {
      def javaBin(java_home: String) = java_home + "/bin/" + Shell.progName(javaCmdName)

      def hasJavaCommand(java_home: String): Boolean = {
        val java_path = new File(javaBin(java_home))
        java_path.exists()
      }

      val java_home: Option[String] = {
        val e = findJavaHome

        import OSType._
        e match {
          case Some(x) => e
          case None => {
            def listJDKIn(path: String) = {
              // TODO Oracle JVM (JRockit) support
              new File(path).listFiles().
                filter(x => x.isDirectory
                && (x.getName.startsWith("jdk") || x.getName.startsWith("jre"))
                && hasJavaCommand(x.getAbsolutePath)).map(_.getAbsolutePath)
            }
            def latestJDK(jdkPath: Array[String]): Option[String] = {
              if (jdkPath.isEmpty)
                None
              else {
                // TODO parse version number
                val sorted = jdkPath.sorted.reverse
                Some(sorted(0))
              }
            }
            debug("No java command found. Searching for JDK...")


            OS.getType match {
              case Windows => latestJDK(listJDKIn("c:/Program Files/Java"))
              case Mac => {
                val l = Seq("/System/Library/Frameworkds/JavaVM.framework/Home", "/System/Library/Frameworkds/JavaVM.framework/Versions/CurrentJDK/Home").
                  filter(hasJavaCommand)
                if (l.isEmpty)
                  None
                else
                  Some(l(0))
              }
              case _ => None
            }
          }
        }
      }

      val ret: Option[String] = java_home match {
        case Some(x) => Some(javaBin(x).trim)
        case None => {
          val javaPath = (Process("which %s".format(javaCmdName)) !!).trim
          if (javaPath.isEmpty)
            None
          else
            Some(javaPath)
        }
      }
      ret
    }

    cmdPathCache.getOrElseUpdate(javaCmdName, search)
  }

}