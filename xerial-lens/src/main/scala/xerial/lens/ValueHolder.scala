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
  def set(path:String, value:String) : ValueHolder = set(Path(path), value)
  def set(path:Path, value:String) : ValueHolder
  def get(path:Path) : Seq[String]
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
        throw new IllegalStateException("path is not found")
      else
        child(path.head).get(path.tailPath)
    }

  }

  private case class Leaf(value:String) extends ValueHolder {
    override def toString = value
    def set(path: Path, value: String) = {
      if(path.isEmpty)
        SeqLeaf(Seq(this.value, value))
      else
        throw new IllegalStateException("cannot add a child to a leaf node")
    }

    def get(path: Path) = {
      if(path.isEmpty)
        Seq(value)
      else
        throw new NoSuchElementException(path.toString)
    }
  }

  private case class SeqLeaf(seq:Seq[String]) extends ValueHolder {
    override def toString = "[%s]".format(seq.mkString(", "))

    def set(path: Path, value: String) = {
      if(path.isEmpty)
        SeqLeaf(seq :+ value)
      else
        throw new IllegalStateException("cannot add a child to a seq node")
    }

    def get(path: Path) =
      if(path.isEmpty)
        seq
      else
        throw new NoSuchElementException(path.toString)
  }

}