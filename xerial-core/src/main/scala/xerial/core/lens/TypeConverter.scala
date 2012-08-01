//--------------------------------------
//
// TypeConverter.scala
// Since: 2012/07/19 3:49 PM
//
//--------------------------------------

package xerial.core.lens


import collection.mutable
import java.text.DateFormat
import java.io.File

/**
 * @author leo
 */
object TypeConverter {

  import TypeUtil._
  import java.lang.{reflect=>jr}

  def convert(value: Any, targetType: ObjectType): Any = {
    if (targetType.isOption) {
      if (isOption(value.getClass))
        value
      else {
        val gt: Seq[ObjectType] = targetType.asInstanceOf[GenericType].genericTypes
        Some(convert(value, gt(0)))
      }
    }
    else if (isArray(targetType.rawType) && isArray(value.getClass)) {
      value
    }
    else {
      val t: Class[_] = targetType.rawType
      val s: Class[_] = value.getClass
      if (t.isAssignableFrom(s))
        value
      else if (TypeUtil.isBuffer(s)) {
        val buf = value.asInstanceOf[mutable.Buffer[_]]
        val gt: Seq[ObjectType] = targetType.asInstanceOf[GenericType].genericTypes
        val e = gt(0).rawType
        type E = e.type
        if (TypeUtil.isArray(t)) {
          val arr = e.newArray(buf.length).asInstanceOf[Array[Any]]
          buf.copyToArray(arr)
          arr
        }
        else if (isSeq(t)) {
          buf.toSeq
        }
        else if (isSet(t)) {
          buf.toSet
        }
        else if (isMap(t)) {
          buf.asInstanceOf[mutable.Buffer[(_, _)]].toMap
        }
        else
          throw sys.error("cannot convert %s to %s".format(s.getSimpleName, t.getSimpleName))
      }
      else
        convert(value, targetType.rawType)
    }
  }

  /**
   * Convert the input value into the target type
   */
  def convert[A](value: Any, targetType: Class[A]): A = {
    val cl: Class[_] = value.getClass
    if (targetType.isAssignableFrom(cl))
      value.asInstanceOf[A]
    else {
      stringConstructor(targetType) match {
        case Some(cc) => cc.newInstance(value.toString).asInstanceOf[A]
        case None => convertToPrimitive(value, Primitive(targetType))
      }
    }
  }

  /**
   * Convert the input value into the target type
   */
  def convertToPrimitive[A](value: Any, targetType: ObjectType): A = {
    val s = value.toString
    val v: Any = targetType match {
      case TextType.String => s
      case Primitive.Boolean => s.toBoolean
      case Primitive.Int => s.toInt
      case Primitive.Float => s.toFloat
      case Primitive.Double => s.toDouble
      case Primitive.Long => s.toLong
      case Primitive.Short => s.toShort
      case Primitive.Byte => s.toByte
      case Primitive.Char if (s.length == 1) => s(0)
      case TextType.File => new File(s)
      case TextType.Date => DateFormat.getDateInstance.parse(s)
      case _ =>
        throw new IllegalArgumentException("""Failed to convert "%s" to %s""".format(s, targetType.toString))
    }
    v.asInstanceOf[A]
  }

  def stringConstructor(cl: Class[_]): Option[jr.Constructor[_]] = {
    val cc = cl.getDeclaredConstructors
    cc.find {
      cc =>
        val pt = cc.getParameterTypes
        pt.length == 1 && pt(0) == classOf[String]
    }
  }



}