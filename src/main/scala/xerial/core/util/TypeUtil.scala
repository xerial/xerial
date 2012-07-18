package xerial.core.util


import collection.mutable
import java.io.File
import java.text.DateFormat
import lens.{Primitive, Type, BasicType, ObjectSchema}
import mutable.ArrayBuffer
import xerial.core.util.lens.ObjectSchema.{GenericType, ValueType}
import java.lang.{reflect=>jr}


//--------------------------------------
//
// TypeUtil.scala
// Since: 2012/07/17 22:47
//
//--------------------------------------

/**
 * @author leo
 */
object TypeUtil {

  implicit def toClassManifest[T](targetType: Class[T]): ClassManifest[T] = ClassManifest.fromClass(targetType)

  def isPrimitive[T](cl:Class[T]) = Primitive.isPrimitive(cl)

  def isArray[T](cl: Class[T]) = {
    cl.isArray || cl.getSimpleName == "Array"
  }

  def isOption[T](cl: ClassManifest[T]): Boolean = {
    val name = cl.erasure.getSimpleName
    // Option None is an object ($)
    name == "Option" || name == "Some" || name == "None$"
  }

  def isBuffer[T](cl: ClassManifest[T]) = {
    cl <:< classOf[mutable.Buffer[_]]
  }

  def isSeq[T](cl: ClassManifest[T]) = {
    cl <:< classOf[Seq[_]] || isArray(cl.erasure)
  }

  def isMap[T](cl: ClassManifest[T]) = {
    cl <:< classOf[Map[_, _]]
  }

  def isSet[T](cl: ClassManifest[T]) = {
    cl <:< classOf[Set[_]]
  }

  def isTuple[T](cl: ClassManifest[T]) = {
    cl <:< classOf[Product]
  }


  def isTraversable[T](cl:ClassManifest[T]) = cl <:< classOf[Traversable[_]]

  def isTraversableOnce[T](cl: ClassManifest[T]) = cl <:< classOf[TraversableOnce[_]]

  def toBuffer[A](input: Array[A]): collection.mutable.Buffer[A] = {
    input.toBuffer[A]
  }


  /**
   * Convert immutable collections or arrays to a mutable buffer
   * @param input
   * @param valueType
   */
  def toBuffer(input: Any, valueType: ObjectSchema.ValueType): collection.mutable.Buffer[_] = {

    def err = throw new IllegalArgumentException("cannot convert to ArrayBuffer: %s".format(valueType))

    if (!canBuildFromBuffer(valueType.rawType))
      err

    val cl: Class[_] = input.getClass
    if (isArray(cl)) {
      val a = input.asInstanceOf[Array[_]]
      a.toBuffer
    }
    else if (isTraversableOnce(cl) && valueType.isGenericType) {
      val gt = valueType.asInstanceOf[ObjectSchema.GenericType]
      val e = gt.genericTypes(0).rawType
      type E = e.type
      val l = input.asInstanceOf[TraversableOnce[E]]
      val b = new ArrayBuffer[E]
      l.foreach(b += _)
      b
    }
    else
      err
  }


  def elementType[T](cl: Class[T]) = {
    cl.getComponentType
  }

  def companionObject[A](cl: Class[A]): Option[Any] = {
    try {
      val companion = Class.forName(cl.getName + "$")
      val companionObj = companion.newInstance()
      Some(companionObj)
    }
    catch {
      case _ => None
    }
  }

  def hasDefaultConstructor[A](cl: Class[A]) = {
    cl.getConstructors.find(x => x.getParameterTypes.length == 0).isDefined
  }

  def canInstantiate[A](cl: Class[A]): Boolean = {
    if (isPrimitive(cl) || hasDefaultConstructor(cl))
      return true

    val fields = cl.getDeclaredFields
    val c = cl.getConstructors().find {
      x =>
        val p = x.getParameterTypes
        if (p.length != fields.length)
          return false

        fields.zip(p).forall(e =>
          e._1.getType == e._2)
    }

    c.isDefined
  }

  def canBuildFromBuffer[T](cl: ClassManifest[T]) = isArray(cl.erasure) || isSeq(cl) || isMap(cl) || isSet(cl)

  def stringConstructor(cl: Class[_]): Option[jr.Constructor[_]] = {
    val cc = cl.getDeclaredConstructors
    cc.find {
      cc =>
        val pt = cc.getParameterTypes
        pt.length == 1 && pt(0) == classOf[String]
    }
  }


  def zero[A](cl: Class[A]): A = {
    if (isPrimitive(cl)) {
      val v: Any = Primitive(cl) match {
        case Primitive.Boolean => true
        case Primitive.Int => 0
        case Primitive.Float => 0f
        case Primitive.Double => 0.0
        case Primitive.Long => 0L
        case Primitive.Short => 0.toShort
        case Primitive.Byte => 0.toByte
        case Primitive.Char => 0.toChar
      }
      v.asInstanceOf[A]
    }
    else if(BasicType.isBasicType(cl)) {
      val v:Any = BasicType(cl) match {
        case BasicType.String => ""
        case BasicType.Date => new java.util.Date(0)
        case BasicType.File => new File("")
      }
      v.asInstanceOf[A]
    }
    else if (isArray(cl)) {
      elementType(cl).newArray(0).asInstanceOf[A]
    }
    else if (isMap(cl)) {
      Map.empty.asInstanceOf[A]
    }
    else if (isSeq(cl)) {
      Seq.empty.asInstanceOf[A]
    }
    else if (isSet(cl)) {
      Set.empty.asInstanceOf[A]
    }
    else if (isOption(cl)) {
      None.asInstanceOf[A]
    }
    else if (isTuple(cl)) {
      val c = cl.getDeclaredConstructors()(0)
      val elementType = cl.getTypeParameters
      val arity = elementType.length
      val args = for (i <- 1 to arity) yield {
        val m = cl.getMethod("_%d".format(i))
        zero(m.getReturnType).asInstanceOf[AnyRef]
      }
      newInstance(cl, args.toSeq)
    }
    else if (canInstantiate(cl)) {
      newInstance(cl).asInstanceOf[A]
    }
    else
      null.asInstanceOf[A]
  }

  def convert(value: Any, targetType: ObjectSchema.ValueType): Any = {
    if (targetType.isOption) {
      if (isOption(value.getClass))
        value
      else {
        val gt: Seq[ValueType] = targetType.asInstanceOf[GenericType].genericTypes
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
      else if (isBuffer(s)) {
        val buf = value.asInstanceOf[mutable.Buffer[_]]
        val gt: Seq[ValueType] = targetType.asInstanceOf[GenericType].genericTypes
        val e = gt(0).rawType
        type E = e.type
        if (isArray(t)) {
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
  def convertToPrimitive[A](value: Any, targetType: Type): A = {
    val s = value.toString
    val v: Any = targetType match {
      case BasicType.String => s
      case Primitive.Boolean => s.toBoolean
      case Primitive.Int => s.toInt
      case Primitive.Float => s.toFloat
      case Primitive.Double => s.toDouble
      case Primitive.Long => s.toLong
      case Primitive.Short => s.toShort
      case Primitive.Byte => s.toByte
      case Primitive.Char if (s.length == 1) => s(0)
      case BasicType.File => new File(s)
      case BasicType.Date => DateFormat.getDateInstance.parse(s)
      case _ =>
        throw new IllegalArgumentException("""Failed to convert "%s" to %s""".format(s, targetType.toString))
    }
    v.asInstanceOf[A]
  }



  def defaultConstructorParameters[A](cl: Class[A]): Seq[AnyRef] = {
    val cc = cl.getConstructors()(0)
    val p = cc.getParameterTypes

    // Search for default parameter values
    //val hasOuter = cl.getDeclaredFields.find(x => x.getName == "$outer").isDefined
    //val numParamStart = if (hasOuter) 1 else 0
    val companion = companionObject(cl)
    val paramArgs = for (i <- 0 until p.length) yield {
      val defaultValue =
        if (companion.isDefined) {
          val methodName = "init$default$%d".format(i + 1)
          try {
            val m = companion.get.getClass.getDeclaredMethod(methodName)
            m.invoke(companion.get)
          }
          catch {
            // When no method for the initial value is found, use 'zero' value of the type
            case e => {
              zero(p(i))
            }
          }
        }
        else
          zero(p(i))
      defaultValue.asInstanceOf[AnyRef]
    }
    paramArgs
  }

  def newInstance[A, B <: AnyRef](cl: Class[A], args: Seq[B]): A = {
    val cc = cl.getConstructors()(0)
    val obj = cc.newInstance(args: _*)
    obj.asInstanceOf[A]
  }

  def newInstance[A](cl: Class[A]): A = {
    def createDefaultInstance: A = {
      val hasOuter = cl.getDeclaredFields.find(x => x.getName == "$outer").isDefined
      if (hasOuter)
        throw new IllegalArgumentException("Cannot instantiate the inner class %s. Use classes defined globally or in companion objects".format(cl.getName))
      val paramArgs = defaultConstructorParameters(cl)
      val cc = cl.getConstructors()(0)
      val obj = cc.newInstance(paramArgs: _*)
      obj.asInstanceOf[A]
    }

    try {
      val c = cl.getConstructor()
      cl.newInstance.asInstanceOf[A]
    }
    catch {
      case e: NoSuchMethodException => createDefaultInstance
    }
  }

}