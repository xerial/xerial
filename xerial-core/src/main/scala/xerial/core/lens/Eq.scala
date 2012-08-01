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

package xerial.core.lens

import xerial.core.log.Logging
import javassist._


//--------------------------------------
//
// HashKey.scala
// Since: 2012/04/02 15:47
//
//--------------------------------------

/**
 * Add value based [Any#equals] and [Any#hashCode] support to an arbitrary class
 *
 * @author leo
 */
trait Eq {

  override lazy val hashCode = {
    val schema = ObjectSchema(Eq.cls(this))
    val hash = schema.parameters.foldLeft(0) {
      (hash, p) =>
        val value = p.get(this)
        (hash * 31) + (if (value != null) value.hashCode() else 0)
    }
    hash % 1907
  }

  override def equals(other: Any) = {
    if (other != null && Eq.cls(this) == Eq.cls(other)) {
      if (this eq other.asInstanceOf[AnyRef]) // if two object refs are identical
        true
      else {
        val schema = ObjectSchema(Eq.cls(this))
        // search for non-equal parameters
        val eq = schema.parameters.find(p =>
          !p.get(this).equals(p.get(other))
        )
        // if isEmpty is true, all parameters are the same
        eq.isEmpty
      }
    }
    else
      false
  }
}

trait FastEq {
  override def equals(other: Any) = {
    if (other != null && this.getClass == other.getClass) {
      if (this eq other.asInstanceOf[AnyRef]) // if two object refs are identical
        true
      else
        EqGen.compare(Eq.cls(this), this, other.asInstanceOf[AnyRef])
    }
    else
      false
  }

}

import java.lang.{reflect => jr}

trait HasEq {
  def compare(a: AnyRef, b: AnyRef): Boolean
}

object Eq  {

  def cls[A](a: A): Class[_] = a.asInstanceOf[AnyRef].getClass
}

object EqGen extends Logging {
  def buildEqCode(cl: Class[_]): String = {
    val schema = ObjectSchema(cl)
    val cmpCode = for (p <- schema.parameters) yield {
      if(Primitive.isPrimitive(p.valueType.rawType))
        "if(diff) return false; else{diff = a.%s() != b.%s();};".format(p.name, p.name)
      else
        "if(diff) return false; else{diff = !a.%s().equals(b.%s());};".format(p.name, p.name)
    }
    val n = cl.getName
    val b = new StringBuilder
    b append "public boolean compare(Object ao, Object bo) {\n"
//    b append """ System.out.println("hello compare: " + a.toString() + " cmp " + b.toString() );""" + "\n";
    b append " %s a = (%s) ao; \n".format(n, n)
    b append " %s b = (%s) bo; \n".format(n, n)
    b append " boolean diff = false;\n "
    b append cmpCode.mkString("\n ")
    b append " return !diff;\n"
    b append "}\n"
    b.result
  }

  private val eqMethodCache = collection.mutable.HashMap[Class[_], HasEq]()

  def eqMethod(cl: Class[_]) = {
    eqMethodCache.getOrElseUpdate(cl, {
      val p = ClassPool.getDefault
      p.appendClassPath(new LoaderClassPath(cl.getClassLoader))
      val c = p.makeClass(cl.getName + "$Eq")
      c.setInterfaces(Array(p.get(classOf[HasEq].getName)))
      val code = buildEqCode(cl)
      trace(code)
      val m = CtNewMethod.make(code, c)
      c.addMethod(m)
      val cmpCls = c.toClass
      val h = cmpCls.newInstance.asInstanceOf[HasEq]
      //cmpCls.getMethod("cmp", cl, cl)
      //debug(h.getClass.getMethods.mkString(","))
      h
    })
  }

  def compare(cl: Class[_], a: AnyRef, b: AnyRef): Boolean = {
    //eqMethod(cl).invoke(null, a, b).asInstanceOf[Boolean]
    eqMethod(cl).compare(a, b)
  }

}