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

package xerial.cui

import xerial.core.XerialSpec
import xerial.core.log.{Logger, LogLevel}

/**
 * @author leo
 */
class LauncherTest extends XerialSpec {

  import Launcher._

  "Launcher" should {

    "populate arguments in constructor" in {
      val l = Launcher.execute[SampleMain]("-h -l debug")
      l.help should be (true)
      l.loglevel should be (Some(LogLevel.DEBUG))
    }


  }
}

class SampleMain(@option(symbol="h", description="display help messages")
                 val help:Boolean=false,
                 @option(symbol="l", description="log level")
                 val loglevel:Option[LogLevel]=None)
  extends Logger
{
  info("Hello launcher")
}