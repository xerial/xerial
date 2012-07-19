package xerial.core.util.lens

import java.{lang => jl}

//--------------------------------------
//
// Primitive.scala
// Since: 2012/07/17 22:26
//
//--------------------------------------

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


sealed abstract class Primitive(val cl: Class[_]) extends Type


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

sealed abstract class BasicType(val cl: Class[_]) extends Type


class MapType(cl: Class[_], keyType: Type, valueType: Type) extends Type
class SeqType(cl: Class[_], elementType: Type) extends Type
class ArrayType(cl: Class[_], elementType: Type) extends Type
class OptionType(cl: Class[_], elementType: Type) extends Type
class TupleType(cl: Class[_], elementType: Seq[Type]) extends Type

class OtherType(cl: Class[_]) extends Type
