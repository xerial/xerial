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
// Module.scala
// Since: 2012/11/08 5:51 PM
//
//--------------------------------------

package xerial.cui

/**
 *
 * {{{
 * // Define your command set
 * class MyCommandSet {
 *    \@command
 *    def hello = println("hello")
 *
 *    \@command
 *    def world = println("world")
 * }
 *
 * // Integrate the above command set as a module with a given name.
 * // Command can be invoked as "sample hello" and "sample world".
 * class MyModule extends Module {
 *   def modules = Map("sample" -> classOf[MyCommandSet])
 * }
 *
 * Launcher[MyModule].execute("sample hello") // prints hello
 *
 * }}}
 *
 *
 * @author leo
 */
trait Module {
  def modules : Map[String, Class[_]]

  /**
   * Place holder for the executed module
   */
  var executedModule : Option[(String, AnyRef)] = None

}