package xerial.lens

import collection.mutable
import java.io.File
import java.text.DateFormat
import mutable.ArrayBuffer
import java.lang.{reflect => jr}
import xerial.core.log.Logger


//--------------------------------------
//
// TypeUtil.scala
// Since: 2012/07/17 22:47
//
//--------------------------------------

/**
 * @author leo
 */
object TypeUtil extends Logger {

  implicit def toClassManifest[T](targetType: Class[T]): ClassManifest[T] = ClassManifest.fromClass(targetType)

  @inline def cls[A](obj:A) : Class[_] = obj.asInstanceOf[AnyRef].getClass
  
  def isPrimitive[T](cl: Class[T]) = Primitive.isPrimitive(cl)

  def isArray[T](cl: Class[T]) = {
    cl.isArray || cl.getSimpleName == "Array"
  }

  /**
   * If the class has unapply(s:String) : T method in the companion object for instantiating class T, returns true.
   * @param cl
   * @tparam T
   * @return
   */
  def hasStringUnapplyConstructor[T](cl:Class[T]) : Boolean = {
    companionObject(cl) map { co =>
      cls(co).getDeclaredMethods.find{ p =>
        def acceptString = {
          val t = p.getParameterTypes
          t.length == 1 && t(0) == classOf[String]
        }
        def returnOptionOfT = {
          val rt = p.getGenericReturnType
          val t = Reflect.getTypeParameters(rt)
          isOption(p.getReturnType) && t.length == 1 && t(0) == cl
        }

        p.getName == "unapply" && acceptString && returnOptionOfT
      } isDefined
    } getOrElse (false)
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


  def isTraversable[T](cl: ClassManifest[T]) = cl <:< classOf[Traversable[_]]

  def isTraversableOnce[T](cl: ClassManifest[T]) = cl <:< classOf[TraversableOnce[_]]

  def toBuffer[A](input: Array[A]): collection.mutable.Buffer[A] = {
    input.toBuffer[A]
  }


  /**
   * Convert immutable collections or arrays to a mutable buffer
   * @param input
   * @param valueType
   */
  def toBuffer(input: Any, valueType: ObjectType): collection.mutable.Buffer[_] = {

    def err = throw new IllegalArgumentException("cannot convert to ArrayBuffer: %s".format(valueType))

    if (!canBuildFromBuffer(valueType.rawType))
      err

    val cl: Class[_] = cls(input)
    if (isArray(cl)) {
      val a = input.asInstanceOf[Array[_]]
      a.toBuffer
    }
    else if (isTraversableOnce(cl) && valueType.isGenericType) {
      val gt = valueType.asInstanceOf[GenericType]
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
      val clName = cl.getName
      val companionCls = if(clName.endsWith("$")) cl else Class.forName(clName + "$")
      val module = companionCls.getField("MODULE$")
      val companionObj = module.get(null)
      Some(companionObj)
    }
    catch {
      case e => {
        warn("no companion object is found for %s: %s", cl, e)
        None
      }
    }
  }

  def hasDefaultConstructor[A](cl: Class[A]) = {
    cl.getConstructors.find(x => x.getParameterTypes.length == 0).isDefined
  }

  def canInstantiate[A](cl: Class[A]): Boolean = {
    if (isPrimitive(cl) || hasDefaultConstructor(cl))
      return true

    val fields = cl.getDeclaredFields
    debug("fields: %s", fields.mkString(", "))
    val c = cl.getConstructors().find {
      x =>
        val p = x.getParameterTypes
        debug("parameter types: %s", p.mkString(", "))
        if (p.length != fields.length)
          return false

        debug("here")
        fields.zip(p).forall(e =>
          e._1.getType == e._2)
    }

    c.isDefined
  }

  def canBuildFromBuffer[T](cl: ClassManifest[T]) = isArray(cl.erasure) || isSeq(cl) || isMap(cl) || isSet(cl)


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
    else if (TextType.isTextType(cl)) {
      val v: Any = TextType(cl) match {
        case TextType.String => ""
        case TextType.Date => new java.util.Date(0)
        case TextType.File => new File("")
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
            val m = cls(companion.get).getDeclaredMethod(methodName)
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