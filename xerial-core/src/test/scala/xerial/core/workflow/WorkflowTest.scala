//--------------------------------------
//
// WorkflowTest.scala
// Since: 2012/08/28 11:17 AM
//
//--------------------------------------

package xerial.core.workflow

import xerial.core.XerialSpec
import xerial.core.io.text.LineReader
import xerial.core.workflow.Workflow.Rule

/**
 * @author leo
 */
class WorkflowTest extends XerialSpec {

  import Workflow._

  class MyWorkflow extends Workflow {

    def inputFiles =
      rule := Seq("A.txt", "B.txt", "C.txt")

    def lc =
      rule := FunRef(lineCount) <= inputFiles


    //def r1 =
//      rule := cat | grep | sort


    def lineCount(in:Seq[String]) = {
      for(each <- in) yield {
        val reader = LineReader(each)
        var count = 0
        while(reader.nextLine.isDefined)
          count += 1
        count
      }
    }


  }


  "Workflow" should {

    "allow defining tasks using Workflow trait" in {
      val w = new MyWorkflow

      debug("%s := %s", w.inputFiles.name, w.inputFiles)
      debug("%s := %s", w.lc.name, w.lc)

    }



  }

}