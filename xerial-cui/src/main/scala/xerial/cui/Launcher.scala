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

import xerial.core.util.CommandLineTokenizer

/**
 * Command launcher
 */
object Launcher {

  def of[A <: AnyRef](implicit m:ClassManifest[A]) : Launcher[A] = {
    new Launcher[A](ClassOptionSchema(m.erasure))
  }

  def execute[A <: AnyRef](argLine:String)(implicit m:ClassManifest[A]) : A = execute(CommandLineTokenizer.tokenize(argLine))(m)
  def execute[A <: AnyRef](args:Array[String])(implicit m:ClassManifest[A]) : A = {
    val l = Launcher.of[A]



    l.execute(args)
  }

}

/**
 * Command launcuer
 *
 * @author leo
 */
class Launcher[A <: AnyRef](schema:OptionSchema)(implicit m:ClassManifest[A]) {

  def execute(argLine:String) : A = execute(CommandLineTokenizer.tokenize(argLine))
  def execute(args:Array[String]) : A = {
    val r = OptionParser.parse(args)(m)
    r.buildObject(m)
  }

  //def addCommand[B](cl:Class[B]) : Launcher[A] = {
//
//  }


}

