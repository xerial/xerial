//--------------------------------------
//
// TypeConverter.scala
// Since: 2012/07/19 3:49 PM
//
//--------------------------------------

package xerial.lens


import collection.mutable
import java.text.DateFormat
import java.io.File
import xerial.core.log.Logger

/**
 * @author leo
 */
object TypeConverter extends Logger {

  import TypeUtil._
  import java.lang.{reflect=>jr}

  def convert(value: Any, targetType: ObjectType): Option[Any] = {
    if (targetType.isOption) {
      if (isOption(cls(value)))
        Some(value)
      else {
        val gt: Seq[ObjectType] = targetType.asInstanceOf[GenericType].genericTypes
        Some(Some(convert(value, gt(0))))
      }
    }
    else if (isArray(targetType.rawType) && isArray(cls(value))) {
      Some(value)
    }
    else {
      val t: Class[_] = targetType.rawType
      val s: Class[_] = cls(value)
      if (t.isAssignableFrom(s))
        Some(value)
      else if (TypeUtil.isBuffer(s)) {
        val buf = value.asInstanceOf[mutable.Buffer[_]]
        val gt: Seq[ObjectType] = targetType.asInstanceOf[GenericType].genericTypes
        val e = gt(0).rawType
        type E = e.type
        if (TypeUtil.isArray(t)) {
          val arr = e.newArray(buf.length).asInstanceOf[Array[Any]]
          buf.copyToArray(arr)
          Some(arr)
        }
        else if (isSeq(t)) {
          Some(buf.toSeq)
        }
        else if (isSet(t)) {
          Some(buf.toSet)
        }
        else if (isMap(t)) {
          Some(buf.asInstanceOf[mutable.Buffer[(_, _)]].toMap)
        }
        else {
          warn("cannot convert %s to %s", s.getSimpleName, t.getSimpleName)
          None
        }
      }
      else
        convert(value, targetType.rawType)
    }
  }

  /**
   * Convert the input value into the target type
   */
  def convert[A](value: Any, targetType: Class[A]): Option[A] = {
    val cl: Class[_] = cls(value)
    if (targetType.isAssignableFrom(cl))
      Some(value.asInstanceOf[A])
    else if(hasStringUnapplyConstructor(targetType)) {
    // call unapply
      companionObject(targetType) flatMap { co =>
        val m = cls(co).getDeclaredMethod("unapply", Array(classOf[String]):_*)
        val v = m.invoke(co, Array(value.toString):_*).asInstanceOf[Option[A]]
        v match {
          case Some(c) => v
          case None =>
            warn("cannot create an instance of %s from %s", targetType, value)
            None
        }
      }
    }
    else {
      stringConstructor(targetType) match {
        case Some(cc) => Some(cc.newInstance(value.toString).asInstanceOf[A])
        case None => convertToPrimitive(value, Primitive(targetType))
      }
    }
  }

  /**
   * Convert the input value into the target type
   */
  def convertToPrimitive[A](value: Any, targetType: ObjectType): Option[A] = {
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
        warn("""Failed to convert "%s" to %s""", s, targetType.toString)
        None
    }
    Some(v.asInstanceOf[A])
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