package xerial.core.util

//--------------------------------------
//
// Primitive.scala
// Since: 2012/07/17 22:26
//
//--------------------------------------

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
}

class Primitive(cl:Class[_]) {
  val name = this.getClass.getSimpleName.replaceAll("""\$""", "")
}
