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
package xerial.lens

import java.{lang => jl}
import xerial.core.log.Logger
import reflect.ClassTag
import scala.language.existentials
//--------------------------------------
//
// ObjectParameter.scala
// Since: 2012/08/02 19:49
//
//--------------------------------------

/**
 * A base class of field parameters and method parameters
 * @param name
 * @param valueType
 */
sealed abstract class Parameter(val name: String, val valueType: ObjectType) {
  val rawType = valueType.rawType

  override def toString = "%s:%s".format(name, valueType)

  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassTag[T]): Option[T]

  protected def findAnnotationOf[T <: jl.annotation.Annotation](annot: Array[jl.annotation.Annotation])(implicit c: ClassTag[T]): Option[T] = {
    annot.collectFirst {
      case x if (c.runtimeClass isAssignableFrom x.annotationType) => x
    }.asInstanceOf[Option[T]]
  }

  def get(obj: Any): Any
}


/**
 * Represents a constructor parameter
 * @param owner
 * @param fieldOwner
 * @param index
 * @param name
 * @param valueType
 */
case class ConstructorParameter(owner: Class[_], fieldOwner: Option[Class[_]], index: Int, override val name: String, override val valueType: ObjectType) extends Parameter(name, valueType) with Logger {
  lazy val field : jl.reflect.Field =
    if(fieldOwner.isDefined)
      fieldOwner.get.getDeclaredField(name)
    else
      sys.error("no field owner is defined in %s".format(this))

  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassTag[T]) = {
    val cc = owner.getConstructors()(0)
    val annot: Array[jl.annotation.Annotation] = cc.getParameterAnnotations()(index)
    findAnnotationOf[T](annot)
  }

  /**
   * Get the default value of this parameter or
   * @return
   */
  def getDefaultValue : Option[Any] = {
    trace(s"get the default value of ${this}")

    TypeUtil.companionObject(owner).flatMap { companion =>
      def findMethod(name:String) = {
        try {
          Some(TypeUtil.cls(companion).getDeclaredMethod(name))
        }
        catch {
          case e: NoSuchMethodException => None
        }
      }
      // Find Scala methods for retrieving default values. Since Scala 2.10 appply or $lessinit$greater$ can be the prefix
      val m = findMethod("apply$default$%d".format(index + 1)) orElse(findMethod("$lessinit$greater$default$%d".format(index + 1)))
      try
        m.map(_.invoke(companion))
      catch {
        case e : Throwable =>
          None
      }
    }
  }

  def get(obj: Any) = {
    Reflect.readField(obj, field)
  }

}

/**
 * Field defined in a class
 * @param owner
 * @param ref
 * @param name
 * @param valueType
 */
case class FieldParameter(owner: Class[_], ref: Class[_], override val name: String, override val valueType: ObjectType)
  extends Parameter(name, valueType) with Logger {
  lazy val field = {
    try
      owner.getDeclaredField(name)
    catch {
      case _ : Throwable =>
        warn("no such field %s in %s (ref:%s)", name, owner.getSimpleName, ref.getSimpleName)
        null
    }
  }

  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassTag[T]) = {
    field match {
      case null => None
      case field =>
        field.getAnnotation[T](c.runtimeClass.asInstanceOf[Class[T]]) match {
          case null => None
          case a => Some(a.asInstanceOf[T])
        }
    }
  }

  def get(obj: Any) = {
    Reflect.readField(obj, field)
  }
}

/**
 * A method argument
 * @param owner
 * @param index
 * @param name
 * @param valueType
 */
case class MethodParameter(owner: jl.reflect.Method, index: Int, override val name: String, override val valueType: ObjectType)
  extends Parameter(name, valueType) {
  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassTag[T]): Option[T] = {
    val annot: Array[jl.annotation.Annotation] = owner.getParameterAnnotations()(index)
    findAnnotationOf[T](annot)
  }

  def get(obj: Any) = {
    sys.error("get for method parameter is not supported")
  }
}

/**
 * A method defined in a scala class
 * @param owner
 * @param jMethod
 * @param name
 * @param params
 * @param returnType
 */
case class ScMethod(owner: Class[_], jMethod: jl.reflect.Method, name: String, params: Array[MethodParameter], returnType: ObjectType)
  extends ObjectMethod {
  override def toString = "Method(%s#%s, [%s], %s)".format(owner.getSimpleName, name, params.mkString(", "), returnType)

  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassTag[T]): Option[T] = {
    jMethod.getAnnotation(c.runtimeClass.asInstanceOf[Class[T]]) match {
      case null => None
      case a => Some(a.asInstanceOf[T])
    }
  }
  def findAnnotationOf[T <: jl.annotation.Annotation](paramIndex: Int)(implicit c: ClassTag[T]): Option[T] = {
    params(paramIndex).findAnnotationOf[T]
  }

  override def hashCode = {
    owner.hashCode() + name.hashCode()
  }

  def invoke(obj:AnyRef, params:AnyRef*) : Any = {
    jMethod.invoke(obj, params:_*)
  }
}

case class CompanionMethod(owner:Class[_], jMethod:jl.reflect.Method, name:String, params: Array[MethodParameter], returnType: ObjectType)
 extends ObjectMethod with Logger
{
  def findAnnotationOf[T <: jl.annotation.Annotation](implicit c: ClassTag[T]): Option[T] = {
    jMethod.getAnnotation(c.runtimeClass.asInstanceOf[Class[T]]) match {
      case null => None
      case a => Some(a.asInstanceOf[T])
    }
  }
  def findAnnotationOf[T <: jl.annotation.Annotation](paramIndex: Int)(implicit c: ClassTag[T]): Option[T] = {
    params(paramIndex).findAnnotationOf[T]
  }

  override def hashCode = {
    owner.hashCode() + name.hashCode()
  }

  def invoke(obj:AnyRef, params:AnyRef*) : Any = {
    debug("invoking jMethod:%s, owner:%s", jMethod, owner)
    TypeUtil.companionObject(owner).map{ co =>
      debug("found a companion object of %s", owner)
      jMethod.invoke(co, params:_*)
    }.orNull
  }

}

/**
 * Constructor of the class
 * @param cl
 * @param params
 */
case class Constructor(cl: Class[_], params: Array[ConstructorParameter]) extends Type {
  val name = cl.getSimpleName
  override def toString = "Constructor(%s, [%s])".format(cl.getSimpleName, params.mkString(", "))

  def findParameter(name:String) = params.find(_.name == name)

  def newInstance(args: Array[AnyRef]): Any = {
    val cc = cl.getConstructors()(0)
    if (args.isEmpty)
      cc.newInstance()
    else
      cc.newInstance(args: _*)
  }
}


