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
import collection.mutable.ArrayBuffer
import reflect.ClassTag
import xerial.core.log.Logger
import collection.mutable
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

object ObjectType extends Logger {


  private[lens] def mirror = ru.runtimeMirror(Thread.currentThread.getContextClassLoader)

  private val typeTable = mutable.WeakHashMap[ru.Type, ObjectType]()

  def apply[A : TypeTag](obj:A) : ObjectType ={
    obj match {
      case cl:Class[_] => of(cl)
      case t:ru.Type => of(t)
      case _ => of(typeOf[A])
    }
  }

  def of(tpe:ru.Type) : ObjectType = {
    def resolveType = {
      trace(f"ObjectType.of(${tpe})")
      val m =
        (primitiveMatcher orElse
          textMatcher orElse
          typeRefMatcher).orElse[ru.Type, ObjectType] {
        case _ =>
          trace(f"Resolving the unknown type $tpe into AnyRef")
          AnyRefType
      }
      m(tpe)
    }
    typeTable.getOrElseUpdate(tpe, resolveType)
  }

  def primitiveMatcher : PartialFunction[ru.Type, Primitive] = {
    case t if t =:= typeOf[Short] => Primitive.Short
    case t if t =:= typeOf[Boolean] => Primitive.Boolean
    case t if t =:= typeOf[Byte] => Primitive.Byte
    case t if t =:= typeOf[Char] => Primitive.Char
    case t if t =:= typeOf[Int] => Primitive.Int
    case t if t =:= typeOf[Float] => Primitive.Float
    case t if t =:= typeOf[Long] => Primitive.Long
    case t if t =:= typeOf[Double] => Primitive.Double

  }

  def textMatcher : PartialFunction[ru.Type, TextType] = {
    case t if t =:= typeOf[String] => TextType.String
    case t if t =:= typeOf[java.util.Date] => TextType.Date
    case t if t =:= typeOf[java.io.File] => TextType.File
  }

  def typeRefMatcher : PartialFunction[ru.Type, ObjectType] = {
    case t if t =:= typeOf[scala.Any] => AnyRefType
    case tpe @ TypeRef(pre, symbol, typeArgs) =>
      if(typeArgs.isEmpty)
        of(mirror.runtimeClass(tpe))
      else
        GenericType(mirror.runtimeClass(tpe), typeArgs.map(apply(_)))
  }


  def of(cl: Class[_]): ObjectType = {
    if (Primitive.isPrimitive(cl))
      Primitive(cl)
    else if (TextType.isTextType(cl))
      TextType(cl)
    else if (cl.getSimpleName == "$colon$colon") {
      SeqType(cl, AnyRefType)
    }
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
  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassTag[T]): Option[T]
  def findAnnotationOf[T <: jl.annotation.Annotation](paramIndex: Int)(implicit c: ClassTag[T]): Option[T]

  def invoke(obj:AnyRef, params:AnyRef*) : Any
}


/**
 *
 *
 * @param rawType
 */
abstract class ObjectType(val rawType: Class[_]) extends Type {
  val name : String = this.getClass.getSimpleName
  def fullName : String = this.getClass.getName
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


case class StandardType[A](override val rawType:Class[A]) extends ObjectType(rawType) with Logger {

  override val name = rawType.getSimpleName

  lazy val constructorParams : Seq[ConstructorParameter] = {

    val m = ObjectType.mirror
    val classSymbol : ru.ClassSymbol = m.staticClass(rawType.getCanonicalName)
    val cc = classSymbol.typeSignature.declaration(ru.nme.CONSTRUCTOR)
    if(cc.isMethod) {
      val fstParen = cc.asMethod.paramss.headOption.getOrElse(Seq.empty)
      for((p, i) <- fstParen.zipWithIndex) yield {
        val name = p.name.decoded
        val tpe = ObjectType(p.typeSignature)
        ConstructorParameter(rawType, ObjectSchema.findFieldOwner(name, rawType), i, name, tpe)
      }
    }
    else
      Seq.empty

  }


}


object GenericType {

  def apply(cl:Class[_], typeArgs:Seq[ObjectType]) : GenericType = {
    import TypeUtil._
    if(TypeUtil.isArray(cl) && typeArgs.length == 1) {
      ArrayType(cl, typeArgs(0))
    }
    else if(TypeUtil.isOption(cl) && typeArgs.length == 1) {
      OptionType(cl, typeArgs(0))
    }
    else if(TypeUtil.isMap(cl) && typeArgs.length == 2) {
      MapType(cl, typeArgs(0), typeArgs(1))
    }
    else if(TypeUtil.isSet(cl) && typeArgs.length == 1) {
      SetType(cl, typeArgs(0))
    }
    else if(TypeUtil.isTuple(cl)) {
      TupleType(cl, typeArgs)
    }
    else if(TypeUtil.isSeq(cl) && typeArgs.length == 1) {
      SeqType(cl, typeArgs(0))
    }
    else if(TypeUtil.isEither(cl) && typeArgs.length == 2) {
      EitherType(cl, typeArgs(0), typeArgs(1))
    }
    else if(TypeUtil.isParSeq(cl) && typeArgs.length == 1) {
      ParSeqType(cl, typeArgs(0))
    }
    else
      new GenericType(cl, typeArgs)
  }
}

class GenericType(override val rawType: Class[_], val genericTypes: Seq[ObjectType]) extends ObjectType(rawType) {
  override val name = "%s[%s]".format(rawType.getSimpleName, genericTypes.map(_.name).mkString(", "))

  override def isOption = rawType == classOf[Option[_]]
  override def isBooleanType = isOption && genericTypes(0).isBooleanType
  override def isGenericType = true
}

case class MapType[A](cl: Class[A], keyType: ObjectType, valueType: ObjectType) extends GenericType(cl, Seq(keyType, valueType))
case class SetType[A](cl: Class[A], elementType: ObjectType) extends GenericType(cl, Seq(elementType))
case class SeqType[A](cl: Class[A], elementType: ObjectType) extends GenericType(cl, Seq(elementType))
case class ParSeqType[A](cl: Class[A], elementType: ObjectType) extends GenericType(cl, Seq(elementType))
case class ArrayType[A](cl: Class[A], elementType: ObjectType) extends GenericType(cl, Seq(elementType))
case class OptionType[A](cl: Class[A], elementType: ObjectType) extends GenericType(cl, Seq(elementType))
case class EitherType[A](cl: Class[A], leftType:ObjectType, rightType:ObjectType) extends GenericType(cl, Seq(leftType, rightType))
case class TupleType[A](cl: Class[A], elementType: Seq[ObjectType]) extends GenericType(cl, elementType)


case object AnyRefType extends ObjectType(classOf[AnyRef]) {
  override val name = "AnyRef"
}



