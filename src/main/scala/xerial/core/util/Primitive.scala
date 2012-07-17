package xerial.core.util

import java.{lang=>jl}

//--------------------------------------
//
// Primitive.scala
// Since: 2012/07/17 22:26
//
//--------------------------------------

trait Type

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

  val javaPrimitiveObjectTypes =
    Set[Class[_]](classOf[jl.Integer], classOf[jl.Short], classOf[jl.Long], classOf[jl.Character],
      classOf[jl.Float], classOf[jl.Byte], classOf[jl.Double], classOf[jl.Boolean], classOf[jl.String]
      //  , jl.Integer.TYPE, jl.Short.TYPE, jl.Character.TYPE, jl.Long.TYPE, jl.Float.TYPE, jl.Byte.TYPE, jl.Boolean.TYPE
    )
  val scalaPrimitiveTypes: Set[Class[_]] =
    Set[Class[_]](classOf[Int], classOf[Short], classOf[Long], classOf[Float], classOf[Byte],
      classOf[Double], classOf[Boolean])

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
    b += classOf[Character] -> Char
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
  

  def apply(cl:Class[_]) : Primitive = primitiveTable(cl)

}

trait Enum {
  val name = this.getClass.getSimpleName.replaceAll("""\$""", "")
}

sealed abstract class Primitive(cl:Class[_]) extends Enum with Type


object BasicType {
  object String extends BasicType(classOf[String])
  object File extends BasicType(classOf[java.io.File])
  object Date extends BasicType(classOf[java.util.Date])

  private val table = Map(
    classOf[String] -> String,
    classOf[java.io.File] -> File,
    classOf[java.util.Date] -> Date
  )

  def apply(cl:Class[_]) : BasicType = table(cl)

  def isBasicType(cl:Class[_]) = table.contains(cl)
}

sealed abstract class BasicType(cl:Class[_]) extends Enum with Type


