package xerial.lens

/*
 * Copyright 2012 Taro L. Saito
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import collection.mutable.{ArrayBuffer}
import xerial.core.log.Logger
import collection.mutable


//--------------------------------------
//
// ObjectBuilder.scala
// Since: 2012/01/25 12:41
//
//--------------------------------------

/**
 *
 *
 */
object ObjectBuilder extends Logger {

  def apply[A](cl: Class[A]): ObjectBuilder[A] = {
    //if (!TypeUtil.canInstantiate(cl))
//      throw new IllegalArgumentException("Cannot instantiate class " + cl)
    new SimpleObjectBuilder(cl)
  }

  sealed trait BuilderElement
  case class Holder(holder:ObjectBuilder[_]) extends BuilderElement
  case class Value(value:Any) extends BuilderElement
  case class ArrayHolder(holder:mutable.ArrayBuffer[Any]) extends BuilderElement


}

trait GenericBuilder {

  def set(path:String, value:Any): Unit = set(Path(path), value)
  def set(path:Path, value:Any): Unit

  def get(name:String) : Option[Any]
}

/**
 * Generic object builder
 * @author leo
 */
trait ObjectBuilder[A] extends GenericBuilder {

  def build: A

}

trait StandardBuilder[ParamType <: Parameter] extends GenericBuilder with Logger {

  import ObjectBuilder._
  import TypeUtil._


  protected val holder = collection.mutable.Map.empty[String, BuilderElement]

  protected def findParameter(name:String) : Option[ParamType]
  protected def getParameterTypeOf(name:String) = findParameter(name).get.valueType

  protected def defaultValues : collection.immutable.Map[String, Any]

  // set the default values of the object
  for((name, value) <- defaultValues) {
    val v : BuilderElement = findParameter(name).map {
      case p if TypeUtil.canBuildFromBuffer(p.rawType) => Value(value)
      case p if canBuildFromStringValue(p.valueType) => Value(value)
      case p => {
        // nested object
        // TODO handling of recursive objects
        val b = ObjectBuilder(p.rawType)
        val schema = ObjectSchema(p.rawType)
        for(p <- schema.constructor.params) {
          b.set(p.name, p.get(value))
        }
        Holder(b)
      }
    } getOrElse Value(value)

    holder += name -> v
  }

  private def canBuildFromStringValue(t:ObjectType) : Boolean = {
    if(TextType.isTextType(t.rawType) || canBuildFromString(t.rawType))
      true
    else
      t match {
        case o:OptionType => canBuildFromStringValue(o.elementType)
        case _ => false
      }
  }

  def set(path: Path, value: Any) {
    if(path.isEmpty) {
      // do nothing
      return
    }
    val name = path.head
    val p = findParameter(name)
    if(p.isEmpty) {
      error("no parameter is found for path %s", path)
      return
    }

    trace("set path %s : %s", path, value)


    if(path.isLeaf) {
      val valueType = p.get.valueType
      trace("update value holder name:%s, valueType:%s (isArray:%s) with value:%s ", name, valueType, TypeUtil.isArray(valueType.rawType), value)
      if (canBuildFromBuffer(valueType.rawType)) {
        val t = valueType.asInstanceOf[GenericType]
        val gt = t.genericTypes(0)

        holder.get(name) match {
          case Some(Value(v)) =>
            // remove the default value
            holder.remove(name)
          case _ => // do nothing
        }
        val arr = holder.getOrElseUpdate(name, ArrayHolder(new ArrayBuffer[Any])).asInstanceOf[ArrayHolder]
        TypeConverter.convert(value, gt) map { arr.holder += _ }
      }
      else if(canBuildFromStringValue(valueType)) {
        TypeConverter.convert(value, valueType).map { v =>
          holder += name -> Value(v)
        }
      }
      else {
        error("failed to set %s to path %s", value, path)
      }
    }
    else {
      // nested object
      val h = holder.getOrElseUpdate(path.head, Holder(ObjectBuilder(p.get.valueType.rawType)))
      h match {
        case Holder(b) => b.set(path.tailPath, value)
        case other =>
          // overwrite the existing holder
          throw new IllegalStateException("invalid path:%s, value:%s, holder:%s".format(path, value, other))
      }
    }
  }

  def get(name:String) : Option[Any] = {
    holder.get(name) flatMap {
      case Holder(h) => Some(h.build)
      case Value(v) => Some(v)
      case ArrayHolder(h) => {
        val p = getParameterTypeOf(name)
        debug("convert array holder:%s into %s", h, p)
        TypeConverter.convert(h, p)
      }
    }
  }
}

class SimpleObjectBuilder[A](cl: Class[A]) extends ObjectBuilder[A] with StandardBuilder[Parameter] with Logger {

  private lazy val schema = ObjectSchema(cl)

  protected def findParameter(name: String) = {
    assert(schema != null)
    schema.findParameter(name)
  }
  protected def defaultValues = {
    // collect default values of the object
    val schema = ObjectSchema(cl)
    val prop = Map.newBuilder[String, Any]

    // get the default values of the constructor
    for(c <- schema.findConstructor; p <- c.params; v <- p.getDefaultValue) {
      trace("set default parameter: %s", p)
      prop += p.name -> v
    }
    val r = prop.result
    trace("class %s. values to set: %s", cl.getSimpleName, r)
    r
  }

  def build: A = {
    trace("holder contents: %s", holder)
    val cc = schema.constructor
    // Prepare constructor args
    val args = (for (p <- cc.params) yield {
      (get(p.name) getOrElse TypeUtil.zero(p.rawType)).asInstanceOf[AnyRef]
    })
    trace("cc:%s, args:%s (size:%d)", cc, args.mkString(", "), args.length)
    cc.newInstance(args).asInstanceOf[A]
  }

}

