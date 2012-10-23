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

import xerial.core.XerialSpec

//--------------------------------------
//
// ShellTest.scala
// Since: 2012/02/06 13:04
//
//--------------------------------------

/**
 * @author leo
 */
class ShellTest extends XerialSpec {
  "Shell" should {
    "find JVM" in {
      val j = Shell.findJavaCommand()
      j should be ('defined)
    }

    "find javaw.exe" in {
      if (OS.isWindows) {
        when("OS is windows")
        val cmd = Shell.findJavaCommand("javaw").get
        cmd must not be (null)
        cmd must include("javaw")
      }
    }

    "detect process IDs" in {
      val p = Shell.launchProcess("echo hello world")
      val pid = Shell.getProcessID(p)
      debug("process ID:%d", pid)
      if(!OS.isWindows) {
        pid should be > (0)
      }
    }

    "be able to launch Java" in {
      Shell.launchJava("-version -Duser.language=en")
    }

    "be able to kill processes" in {
      val p = Shell.launchProcess("cat")
      val pid = Shell.getProcessID(p)
      val exitCode = Shell.kill(pid)
    }


    "find sh" in {
      val cmd = Shell.findSh
      cmd should be ('defined)
    }

    "launch command" in {
      Shell.launchProcess("echo hello world")
      Shell.launchProcess("echo cygwin env=$CYGWIN")
    }

    "launch process" in {
      if (OS.isWindows) {
        when("OS is windows")
        Shell.launchCmdExe("echo hello cmd.exe")
      }
    }
  }


}