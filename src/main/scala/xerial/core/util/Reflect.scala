package xerial.core.util


//--------------------------------------
//
// Reflect.scala
// Since: 2012/07/17 22:45
//
//--------------------------------------

/**
 * Reflection utility functions
 * @author leo
 */
object Reflect {

  import java.lang.{reflect=>jr}

  /**
   * Set the accessibility flag of fields and methods if they are not accessible, then
   * do some operation, and reset the accessibility properly upon the completion.
   */
  private[util] def access[A <: jr.AccessibleObject, B](f: A)(body: => B): B = {
    val accessible = f.isAccessible
    try {
      if (!accessible)
        f.setAccessible(true)
      body
    }
    finally {
      if (!accessible)
        f.setAccessible(false)
    }
  }

  def readField(obj: Any, f: jr.Field): Any = {
    access(f) {
      f.get(obj)
    }
  }

}