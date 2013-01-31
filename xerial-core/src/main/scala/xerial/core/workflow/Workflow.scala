//--------------------------------------
//
// Workflow.scala
// Since: 2012/08/28 11:11 AM
//
//--------------------------------------

package xerial.core.workflow

import xerial.core.log.Logger

/**
 * @author leo
 */
object Workflow {


  sealed abstract class Rule(val name:String) {
    def outputType: Class[_] = null
  }

  case class RuleBase(override val name:String) extends Rule(name) {
    def :=[R](f:R) : FunRule[R] = FunRule(name, f)
    def :=(task:Rule) : Rule = {
      val t = RuleRef(name)
      t.set(task)
      t
    }
  }


  case class FunRule[R](override val name:String, f: R) extends Rule(name) {
    override def toString = "call(%s)".format(name)
  }

  case class RuleRef(override val name:String, private var task:Rule=null) extends Rule(name) {

    def set(task:Rule) { this.task = task }
  }

  case class FunRef[A, B](f:A=>B) {
    def apply(t:Rule*) : Rule = new FunApply(this, t)
    def <=(t:Rule*) : Rule = new FunApply(this, t)  // TODO static type checking
  }

  case class FunApply[A, B](f:FunRef[A,B], input:Seq[Rule]) extends Rule("funapply") {

  }
}


trait Workflow extends Logger {

  import Workflow._

  def rule : RuleBase = RuleBase(getEnclosingMethodName(3))

  //def rule[R](r: R) : Rule = rule(getEnclosingMethodName(3), r)



//  private var ruleCache : Map[String, Rule] = Map[String, Rule]()
//
//  /**
//   * Construct a new expr with a given name
//   * @param ruleName
//   * @param task
//   * @return
//   */
//  def rule[R](ruleName: String, task: R): Rule = {
//    ruleCache.get(ruleName) match {
//      case Some(r) => r
//      case None => {
//        // Insert a reference to this expr first to avoid recursively calling this method
//        val ref = RuleRef(ruleName)
//        ruleCache += ruleName -> ref
//        // Prepare the expr
//        val newTask : Rule = task match {
//          case t:Rule => t
//          case _ => FunRule(task)
//        }
//        // Update the reference
//        ref.set(newTask)
//        debug("Define task %15s := %s", ruleName, newTask)
//        ref
//      }
//    }
//  }


  private def getEnclosingMethodName(stackLevel: Int): String = {
    new Throwable().getStackTrace()(stackLevel).getMethodName
  }

}