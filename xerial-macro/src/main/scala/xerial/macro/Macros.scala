//--------------------------------------
//
// Macros.scala
// Since: 2013/02/12 1:35 PM
//
//--------------------------------------

package xerial.macros
import scala.reflect.macros.Context
import scala.language.experimental.macros


/**
 * Macro definitions
 *
 * @author Taro L. Saito
 */
object Macros {
  def enclosingMethodNameImpl(c:Context) : c.Expr[String] = {
    import c.universe._

    c.enclosingMethod match {
      case DefDef(_, name, _, _, _, _) =>
        c.universe.reify(c.literal(name.toString).splice)
      case _ => c.abort(c.enclosingPosition, "no enclosing method")
    }
  }

  /**
   * Retrieve the enclosing method name. If no enclosing method is found,
   * report the compilation error
   */
  def enclosingMethodName : String = macro enclosingMethodNameImpl



}

