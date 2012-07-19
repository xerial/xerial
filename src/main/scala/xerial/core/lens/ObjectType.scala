//--------------------------------------
//
// ObjectType.scala
// Since: 2012/07/19 3:58 PM
//
//--------------------------------------

package xerial.core.lens

import xerial.core.util.Reflect
import xerial.core.log.Logging

/**
 * @author leo
 */

import java.{lang => jl}

object Type {

  def apply(cl: Class[_]): Type = {
    if (Primitive.isPrimitive(cl))
      Primitive(cl)
    else if (BasicType.isBasicType(cl))
      BasicType(cl)
    else
      new OtherType(cl)
  }

}

trait Type {
  val name : String
}

abstract class ValueType(val rawType: Class[_]) extends Type {
  override val name: String = rawType.getSimpleName

  override def toString = name
  def isOption = false
  def isBooleanType = false
  def isGenericType = false
}

case class StandardType(override val rawType: Class[_]) extends ValueType(rawType) {
  override def isBooleanType = rawType == classOf[Boolean]
}

case class GenericType(override val rawType: Class[_], genericTypes: Seq[ValueType]) extends ValueType(rawType) {
  override def toString = "%s[%s]".format(rawType.getSimpleName, genericTypes.map(_.name).mkString(", "))

  override def isOption = rawType == classOf[Option[_]]
  override def isBooleanType = isOption && genericTypes(0).isBooleanType
  override def isGenericType = true
}

/**
 * A base class of field parameters and method parameters
 * @param name
 * @param valueType
 */
sealed abstract class Parameter(val name: String, val valueType: ValueType) {
  val rawType = valueType.rawType

  override def toString = "%s:%s".format(name, valueType)

  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassManifest[T]): Option[T]

  protected def findAnnotationOf[T <: jl.annotation.Annotation](annot: Array[jl.annotation.Annotation])(implicit c: ClassManifest[T]): Option[T] = {
    annot.collectFirst {
      case x if (c.erasure isAssignableFrom x.annotationType) => x
    }.asInstanceOf[Option[T]]
  }

  def get(obj: Any): Any
}

case class ConstructorParameter(owner: Class[_], fieldOwner: Class[_], index: Int, override val name: String, override val valueType: ValueType) extends Parameter(name, valueType) {
  lazy val field = fieldOwner.getDeclaredField(name)
  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassManifest[T]) = {
    val cc = owner.getConstructors()(0)
    val annot: Array[jl.annotation.Annotation] = cc.getParameterAnnotations()(index)
    findAnnotationOf[T](annot)
  }

  def get(obj: Any) = {
    Reflect.readField(obj, field)
  }

}

case class FieldParameter(owner: Class[_], ref: Class[_], override val name: String, override val valueType: ValueType) extends Parameter(name, valueType) with Logging {
  lazy val field = {
    try
      owner.getDeclaredField(name)
    catch {
      case _ =>
        warn("no such field %s in %s (ref:%s)", name, owner.getSimpleName, ref.getSimpleName)
        null
    }
  }

  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassManifest[T]) = {
    field match {
      case null => None
      case field =>
        field.getAnnotation[T](c.erasure.asInstanceOf[Class[T]]) match {
          case null => None
          case a => Some(a.asInstanceOf[T])
        }
    }
  }

  def get(obj: Any) = {
    Reflect.readField(obj, field)
  }
}

case class MethodParameter(owner: jl.reflect.Method, index: Int, override val name: String, override val valueType: ValueType) extends Parameter(name, valueType) {
  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassManifest[T]): Option[T] = {
    val annot: Array[jl.annotation.Annotation] = owner.getParameterAnnotations()(index)
    findAnnotationOf[T](annot)
  }

  def get(obj: Any) = {
    sys.error("get for method parameter is not supported")
  }
}

case class Method(owner: Class[_], jMethod: jl.reflect.Method, name: String, params: Array[MethodParameter], returnType: Type) extends Type {
  override def toString = "Method(%s#%s, [%s], %s)".format(owner.getSimpleName, name, params.mkString(", "), returnType)

  //lazy val jMethod = owner.getMethod(name, params.map(_.rawType): _*)

  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassManifest[T]): Option[T] = {
    jMethod.getAnnotation(c.erasure.asInstanceOf[Class[T]]) match {
      case null => None
      case a => Some(a.asInstanceOf[T])
    }
  }
  def findAnnotationOf[T <: jl.annotation.Annotation](paramIndex: Int)(implicit c: ClassManifest[T]): Option[T] = {
    params(paramIndex).findAnnotationOf[T]
  }

  override def hashCode = {
    owner.hashCode() + name.hashCode()
  }
}

case class Constructor(cl: Class[_], params: Array[ConstructorParameter]) extends Type {
  val name = cl.getSimpleName
  override def toString = "Constructor(%s, [%s])".format(cl.getSimpleName, params.mkString(", "))

  def newInstance(args: Array[AnyRef]): Any = {
    val cc = cl.getConstructors()(0)
    if (args.isEmpty)
      cc.newInstance()
    else
      cc.newInstance(args: _*)
  }
}


trait SharedType extends Type {
  val name = this.getClass.getSimpleName.replaceAll("""\$""", "")
}

/**
 * Enum of Scala's primitive types
 *
 * @author leo
 */
object Primitive {
  object Boolean extends Primitive(classOf[Boolean])
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


sealed abstract class Primitive(val cl: Class[_]) extends SharedType


object BasicType {
  object String extends BasicType(classOf[String])
  object File extends BasicType(classOf[java.io.File])
  object Date extends BasicType(classOf[java.util.Date])

  val values = Seq(String, File, Date)

  private val table = Map[Class[_], BasicType](
    classOf[String] -> String,
    classOf[java.io.File] -> File,
    classOf[java.util.Date] -> Date
  )

  def apply(cl: Class[_]): BasicType = table(cl)

  def isBasicType(cl: Class[_]) : Boolean = table.contains(cl)
}

sealed abstract class BasicType(val cl: Class[_]) extends SharedType


class MapType(cl: Class[_], keyType: Type, valueType: Type) extends SharedType
class SeqType(cl: Class[_], elementType: Type) extends SharedType
class ArrayType(cl: Class[_], elementType: Type) extends SharedType
class OptionType(cl: Class[_], elementType: Type) extends SharedType
class TupleType(cl: Class[_], elementType: Seq[Type]) extends SharedType

class OtherType(cl: Class[_]) extends SharedType
