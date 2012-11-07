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

//--------------------------------------
//
// ValueHolder.scala
// Since: 2012/11/06 5:40 PM
//
//--------------------------------------

package xerial.lens

import xerial.core.log.Logger

/**
 * Holder of structured data of named strings. ValueHolder is immutable, so
 * set operations return another ValueHolder and never modify the original ValueHolder.
 *
 * <pre>
 * A(a, B(b, c))
 *
 * { a: apple, B:{b:book, c:car} }
 *
 * val n1 = Empty.set("a", apple)  =>  Node(a -> Leaf(apple))
 * val n2 = n1.set("B.b", "book")
 *       => Node(a -> Leaf(apple), B -> Empty.set("b", "book"))
 *       => Node(a -> apple, B->Node(b -> Leaf(book)))
 * val n3 = n2.set("B.c", "car") => Node(a ->apple, B->Node(b -> Leaf(book), c->Leaf(car)))
 *
 * </pre>
 *
 * @author Taro L. Saito
 *
 */
trait ValueHolder {
  /**
   * Set a value at the specified path
   * @param path string representation of [[xerial.lens.Path]]
   * @param value
   * @return updated value holder
   */
  def set(path:String, value:String) : ValueHolder = set(Path(path), value)

  /**
   * Set a value at the specified path
   * @param path path
   * @param value String value to set
   * @return updated value holder
   */
  def set(path:Path, value:String) : ValueHolder

  /**
   * Extract a part of the value holder under the path
   * @param path
   * @return value holder under the path
   */
  def get(path:String) : ValueHolder = get(Path(path))

  /**
   * Extract a part of the value holder under the path
   * @param path
   * @return value holder under the path
   */
  def get(path:Path) : ValueHolder

}


object ValueHolder extends Logger {

  import collection.immutable.{Map => IMap}

  type Holder = IMap[String, ValueHolder]

  def empty : ValueHolder = Empty

  private case object Empty extends ValueHolder {
    def set(path: Path, value: String) = {
      if(path.isEmpty)
        Leaf(value)
      else
        Node(IMap.empty[String, ValueHolder]).set(path, value)
    }
    def get(path:Path) = throw new NoSuchElementException(path.toString)
    def extract(path:Path) = Empty
  }

  private case class Node(child:IMap[String, ValueHolder]) extends ValueHolder {
    override def toString = "{%s}".format(child.map{e => "%s:%s".format(e._1, e._2) }.mkString(", "))

    def set(path: Path, value: String) = {
      if(path.isEmpty)
        throw new IllegalStateException("path cannot be empty")
      else {
        val p = child.getOrElse(path.head, Empty).set(path.tailPath, value)
        Node(child + (path.head -> p))
      }
    }

    def get(path:Path) = {
      if(path.isEmpty)
        this
      else
        child.get(path.head) map { _.get(path.tailPath) } getOrElse Empty
    }
  }

  private case class Leaf(value:String) extends ValueHolder {
    override def toString = value
    def set(path: Path, value: String) = {
      SeqLeaf(Seq(this, Empty.set(path, value)))
    }

    def get(path: Path) = {
      if(path.isEmpty)
        this
      else
        Empty
    }
  }

  private case class SeqLeaf(elems:Seq[ValueHolder]) extends ValueHolder {
    override def toString = "[%s]".format(elems.mkString(", "))

    def set(path: Path, value: String) = {
      SeqLeaf(elems :+ Empty.set(path, value))
    }

    def get(path: Path) =
      if(path.isEmpty)
        this
      else
        Empty
  }

}