//--------------------------------------
//
// JavassistUtil.scala
// Since: 2013/02/06 4:00 PM
//
//--------------------------------------

package xerial.lens

import scala.reflect.runtime.{universe => ru}
import ru._
import javassist._
import scala.Array
import scala.Some
import reflect.runtime._
import scala.Some
import scala.Some
import reflect.ClassTag
import xerial.core.log.Logger


/**
 * Utilities for generating classes using Javassist
 * @author Taro L. Saito
 */
object JavassistUtil {

  /**
   * Create a factory for generating new javassist codes
   * @tparam Key
   * @tparam Interface
   * @return
   */
  def newFactory[Key, Interface: ClassTag]: MethodFactory[Key, Interface] = new MethodFactory[Key, Interface]

  class MethodFactory[Key, Interface](implicit tag: ClassTag[Interface]) extends Logger {

    import collection.JavaConversions._

    private val codeTable = new java.util.concurrent.ConcurrentHashMap[Key, Interface]()

    private val interfaceCls = tag.runtimeClass

    // Using the class loader in which Interface class is defined
    // so that the interface class is visible from a newly generated class
    private val loader = interfaceCls.getClassLoader
    private val pool = ClassPool.getDefault
    pool.appendClassPath(new LoaderClassPath(loader))

    def getOrElseUpdate[U](key: Key, className: => String, methodCodes: => Seq[String]): Interface = {
      codeTable.getOrElseUpdate(key, genCode(className, {
        c: CtClass =>
          for (src <- methodCodes) {
            trace(s"new code:\n$src")
            c.addMethod(CtNewMethod.make(src, c))
          }
      }))
    }

    def getOrElseUpdate[U](key: Key, className: => String, methodGen: CtClass => U): Interface = {
      codeTable.getOrElseUpdate(key, genCode(className, methodGen))
    }

    private def genCode[U](className: String, methodGen: CtClass => U) = {
      synchronized {
        def loadClass: Option[Class[_]] =
          try
            Some(loader.loadClass(className))
          catch {
            case e: ClassNotFoundException =>
              trace(s"Class $className is not found")
              None
          }

        def loadCtClass: Option[CtClass] =
          try
            Some(pool.get(className))
          catch {
            case e: NotFoundException =>
              trace(s"CtClass $className is not found")
              None
          }

        def newCtClass = {
          trace(s"new CtClass $className")
          val c = pool.makeClass(className)
          c.setInterfaces(Array(pool.get(interfaceCls.getName)))
          methodGen(c)
          c
        }

        val cls = loadClass getOrElse {
          loadCtClass getOrElse newCtClass toClass(loader, null)
        }

        cls.newInstance.asInstanceOf[Interface]
      }
    }

  }

}