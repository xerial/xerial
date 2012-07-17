package xerial.core.util

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

  def isArray[T](cl: Class[T]) = {
    cl.isArray || cl.getSimpleName == "Array"
  }

}