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
// ObjectType.scala
// Since: 2012/07/19 3:58 PM
//
//--------------------------------------

package xerial.lens
import java.{lang => jl}

object ObjectType {

  def apply(cl: Class[_]): ObjectType = {
    if (Primitive.isPrimitive(cl))
      Primitive(cl)
    else if (TextType.isTextType(cl))
      TextType(cl)
    else
      StandardType(cl)
  }

}

trait Type {
  val name : String
}

trait ObjectMethod extends Type {

  val params : Array[MethodParameter]
  val jMethod : jl.reflect.Method
  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassManifest[T]): Option[T]
  def findAnnotationOf[T <: jl.annotation.Annotation](paramIndex: Int)(implicit c: ClassManifest[T]): Option[T]

  def invoke(obj:AnyRef, params:AnyRef*) : Any
}


/**
 *
 *
 * @param rawType
 */
abstract class ObjectType(val rawType: Class[_]) extends Type {
  val name : String = this.getClass.getSimpleName
  override def toString = name
  def isOption = false
  def isBooleanType = false
  def isGenericType = false
  def isPrimitive : Boolean = false
}

trait ValueObject extends ObjectType {
  override val name : String = this.getClass.getSimpleName.replaceAll("""\$""", "")
}

/**
 * Scala's primitive types. The classes in this category can create primitive type arrays.
 *
 * @author leo
 */
object Primitive {
  object Boolean extends Primitive(classOf[Boolean]) {
    override def isBooleanType = true
  }
  object Short extends Primitive(classOf[Short])
  object Byte extends Primitive(classOf[Byte])
  object Char extends Primitive(classOf[Char])
  object Int extends Primitive(classOf[Int])
  object Float extends Primitive(classOf[Float])
  object Long extends Primitive(classOf[Long])
  object Double extends Primitive(classOf[Double])

  val values = Seq(Boolean, Short, Byte, Char, Int, Float, Long, Double)

  def isPrimitive(cl: Class[_]): Boolean = {
    cl.isPrimitive || primitiveTable.contains(cl)
  }

  private val primitiveTable = {
    val b = Map.newBuilder[Class[_], Primitive]
    b += classOf[jl.Boolean] -> Boolean
    b += classOf[Boolean] -> Boolean
    b += classOf[jl.Short] -> Short
    b += classOf[Short] -> Short
    b += classOf[jl.Byte] -> Byte
    b += classOf[Byte] -> Byte
    b += classOf[jl.Character] -> Char
    b += classOf[Char] -> Char
    b += classOf[jl.Integer] -> Int
    b += classOf[Int] -> Int
    b += classOf[jl.Float] -> Float
    b += classOf[Float] -> Float
    b += classOf[jl.Long] -> Long
    b += classOf[Long] -> Long
    b += classOf[jl.Double] -> Double
    b += classOf[Double] -> Double
    b.result
  }

  def apply(cl: Class[_]): Primitive = primitiveTable(cl)
}


sealed abstract class Primitive(cl: Class[_]) extends ObjectType(cl) with ValueObject {
  override def isPrimitive = true
}

/**
 * Types that can be constructed from String
 * @param cl
 */
sealed abstract class TextType(cl: Class[_]) extends ObjectType(cl) with ValueObject

object TextType {
  object String extends TextType(classOf[String])
  object File extends TextType(classOf[java.io.File])
  object Date extends TextType(classOf[java.util.Date])

  val values = Seq(String, File, Date)

  private val table = Map[Class[_], TextType](
    classOf[String] -> String,
    classOf[java.io.File] -> File,
    classOf[java.util.Date] -> Date
  )

  def apply(cl: Class[_]): TextType = table(cl)

  def isTextType(cl: Class[_]) : Boolean = table.contains(cl)
}

case class StandardType(override val rawType:Class[_]) extends ObjectType(rawType)


class GenericType(override val rawType: Class[_], val genericTypes: Seq[ObjectType]) extends ObjectType(rawType) {
  override def toString = "%s[%s]".format(rawType.getSimpleName, genericTypes.map(_.name).mkString(", "))

  override def isOption = rawType == classOf[Option[_]]
  override def isBooleanType = isOption && genericTypes(0).isBooleanType
  override def isGenericType = true
}

case class MapType(cl: Class[_], keyType: ObjectType, valueType: ObjectType) extends GenericType(cl, Seq(keyType, valueType))
case class SeqType(cl: Class[_], elementType: ObjectType) extends GenericType(cl, Seq(elementType))
case class ArrayType(cl: Class[_], elementType: ObjectType) extends GenericType(cl, Seq(elementType))
case class OptionType(cl: Class[_], elementType: ObjectType) extends GenericType(cl, Seq(elementType))
case class EitherType(cl: Class[_], leftType:ObjectType, rightType:ObjectType) extends GenericType(cl, Seq(leftType, rightType))
case class TupleType(cl: Class[_], elementType: Seq[ObjectType]) extends GenericType(cl, elementType)






