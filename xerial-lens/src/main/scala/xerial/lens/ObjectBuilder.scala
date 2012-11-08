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
import xerial.lens
import xerial.core.log.Logger


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
  case class ArrayHolder[E : ClassManifest](holder:ArrayBuffer[E]) extends BuilderElement {
    def +=(elem:E) = holder += elem
  }

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
  protected def getParameterTypeOf(name:String) = findParameter(name).map{_.valueType.rawType} getOrElse(classOf[AnyRef])

  protected def defaultValues : collection.immutable.Map[String, Any]

  // set default value of the object
  for((name, value) <- defaultValues) {
    val v = findParameter(name).
      filter(p => TypeUtil.canBuildFromBuffer(p.valueType.rawType)).
      map { x =>
      trace("name:%s valueType:%s", name, x.valueType)
      toBuffer(value, x.valueType)
    } getOrElse value
    holder += name -> Value(v)
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

    if(path.isLeaf) {
      val valueType = p.get.valueType
      trace("update value holder name:%s, valueType:%s (isArray:%s) with value:%s ", name, valueType, TypeUtil.isArray(valueType.rawType), value)
      if (canBuildFromBuffer(valueType.rawType)) {
        val t = valueType.asInstanceOf[GenericType]
        val gt = t.genericTypes(0).rawType
        type E = gt.type

        val arr = holder.getOrElseUpdate(name, ArrayHolder[E](new ArrayBuffer[E])).asInstanceOf[ArrayHolder[E]]
        TypeConverter.convert(value, gt) map { arr += _.asInstanceOf[E] }
      }
      else if(canBuildFromString(valueType.rawType)) {
        holder += name -> Value(TypeConverter.convert(value, valueType.rawType))
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
        case _ => throw new IllegalStateException("invalid path:%s".format(p))
      }
    }
  }

  def get(name:String) : Option[Any] = {
    holder.get(name) map {
      case Holder(h) => Some(h.build)
      case Value(v) => TypeConverter.convert(v, getParameterTypeOf(name))
      case ArrayHolder(h) => TypeConverter.convert(h, getParameterTypeOf(name))
    }
  }
}

class SimpleObjectBuilder[A](cl: Class[A]) extends ObjectBuilder[A] with StandardBuilder[Parameter] with Logger {

  private val schema = ObjectSchema(cl)

  protected def findParameter(name: String) = schema.findParameter(name)
  protected def defaultValues = {
    // collect default values of the object
    val schema = ObjectSchema(cl)
    val prop = Map.newBuilder[String, Any]
    trace("class %s. values to set: %s", cl.getSimpleName, prop)
    // get the default values (including constructor parameters and fields)
    val default = TypeUtil.newInstance(cl)
    for (p <- schema.parameters) {
      trace("set parameter: %s", p)
      prop += p.name -> p.get(default)
    }
    prop.result
  }

  def build: A = {

    def getValue(p: Parameter): Option[_] = {
      val v = holder.getOrElse(p.name, TypeUtil.zero(p.valueType.rawType))
      if (v != null) {
        val cv = TypeConverter.convert(v, p.valueType)
        trace("getValue:%s, v:%s => cv:%s", p, v, cv)
        cv
      }
      else
        None
    }

    val cc = schema.constructor

    // TODO Do we need to set fields not in the constructor arguments? 
    var remainingParams = schema.parameters.map(_.name).toSet

    // Prepare constructor args
    val args = (for (p <- cc.params) yield {
      val v = get(p.name)
      remainingParams -= p.name
      (v getOrElse null).asInstanceOf[AnyRef]
    })
    trace("cc:%s, args:%s", cc, args.mkString(", "))
    val res = cc.newInstance(args).asInstanceOf[A]

    // Set the remaining parameters
    trace("remaining params: %s", remainingParams.mkString(", "))
    for (pname <- remainingParams) {
      schema.getParameter(pname) match {
        case f@FieldParameter(owner, ref, name, valueType) => {
          getValue(f).map {
            Reflect.setField(res, f.field, _)
          }
        }
        case _ => // ignore constructor/method parameters
      }
    }

    res
  }

}

