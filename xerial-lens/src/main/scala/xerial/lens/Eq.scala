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

package xerial.lens

import xerial.core.log.Logger
import javassist._
import xerial.core.util.StringTemplate
import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}


//--------------------------------------
//
// Eq.scala
// Since: 2012/04/02 15:47
//
//--------------------------------------


/**
 * Add value based [Any#equals] and [Any#hashCode] support to an arbitrary class using Reflection.
 * [[xerial.lens.Eq]] trait is a faster version of this class, based on runtime code generation.
 *
 * @author leo
 */
trait EqByReflection {

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

/**
 * Add value based [Any#equals] comparison and [Any#hashCode] support to an arbitrary class.
 *
 * @author leo
 */
trait Eq {
  override def equals(other: Any) = {
    EqGen.compare(Eq.cls(this), this, other.asInstanceOf[AnyRef])
  }

  override lazy val hashCode = {
    EqGen.hash(Eq.cls(this), this)
  }
}


import java.lang.{reflect => jr}

/**
 * A common trait that implements comparator and hash code generation.
 */
trait HasEq {
  def compare(a: AnyRef, b: AnyRef): Boolean

  def genHash(a: AnyRef): Int
}

object Eq {
  def cls[A](a: A): Class[_] = a.asInstanceOf[AnyRef].getClass
}

/**
 * This class generates a code of the equality check and hash code from a given class definition
 */
object EqGen extends Logger {
  def buildEqCode(cl: Class[_]): String = {
    val schema = ObjectSchema(cl)
    val cmpCode = for (p <- schema.parameters) yield {
      if (Primitive.isPrimitive(p.valueType.rawType))
        "if(a.%s() != b.%s()) return false;".format(p.name, p.name)
      else
        "if(!a.%s().equals(b.%s())) return false;".format(p.name, p.name)
    }
    val t = """|public boolean compare(Object ao, Object bo) {
              | if(bo == null || ao.getClass() != bo.getClass())
              |   return false;
              | if(ao == bo) return true;
              | $type$ a = ($type$) ao;
              | $type$ b = ($type$) bo;
              | $cond$
              | return true; }""".stripMargin
    val code = StringTemplate.eval(t)(Map("type" -> cl.getName, "cond" -> cmpCode.mkString("\n else ")))
    trace(s"generated a equality check code:\n$code")
    code
  }

  def buildHashCode(cl: Class[_]): String = {
    val schema = ObjectSchema(cl)
    val getter = for (p <- schema.parameters; n = p.name) yield {
      def default = Seq("(int) v.%s()".format(n))
      def splitDouble = Seq("(int) ((v.%s() >> 32) & 0xFFFFFFFFL)".format(n), "(int) (v.%s() & 0xFFFFFFFFL)".format(n))
      ObjectType(p.valueType.rawType) match {
        case Primitive.Boolean => Seq("v.%s() ? 1 : 0".format(n))
        case Primitive.Int => default
        case Primitive.Short => default
        case Primitive.Char => default
        case Primitive.Byte => default
        case Primitive.Long => splitDouble
        case Primitive.Float => Seq("(int) (v.%s() & ~0)".format(n))
        case Primitive.Double => splitDouble
        // TODO support collection types (e.g., Array, Seq, etc.)
        case _ => Seq("(v.%s() != null) ? v.%s().hashCode() : 0".format(n, n))
      }
    }
    val comp = getter.flatten.map(s => "h *= 31; h += %s;".format(s)).mkString("\n  ")
    val t =
      """|public int genHash(Object o) {
        |  $type$ v = ($type$) o;
        |  int h = 0;
        |  $comp$
        |  return h;
        |}
      """.stripMargin
    val code = StringTemplate.eval(t)(Map("type" -> cl.getName, "comp" -> comp))
    trace(s"generated a hash code:\n$code")
    code
  }

  protected[lens] val codeFactory = JavassistUtil.newFactory[Class[_], HasEq]

  def eqCodeOf(cl: Class[_]) : HasEq =
    codeFactory.getOrElseUpdate(cl, s"${cl.getName}$$Eq", { c =>
      c.addMethod(CtNewMethod.make(buildEqCode(cl), c))
      c.addMethod(CtNewMethod.make(buildHashCode(cl), c))
    })

  def compare(cl: Class[_], a: AnyRef, b: AnyRef): Boolean = eqCodeOf(cl).compare(a, b)

  def hash(cl: Class[_], a: AnyRef): Int = eqCodeOf(cl).genHash(a)

}