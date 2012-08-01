package xerial.core.util

import xerial.core.XerialSpec
import collection.JavaConversions._
//--------------------------------------
//
// ResourceTest.scala
// Since: 2012/07/17 12:06
//
//--------------------------------------

/**
 * @author leo
 */
class ResourceTest extends XerialSpec {

  "Resource" should {

    "find files from the current class loader" in {
      debug("find files from package")
      val l = Resource.listResources("xerial.core.util", { s : String => s.endsWith(".class")})
      l.size should be > 0
    }

    "find resources from jar files" in {
      debug("find files from a jar file")

      val l = Resource.listResources("scala.io", { s: String => s.endsWith(".class")} )
      l.size should be > 0
      for(each <- l) {
        each.url.toString should include ("/scala/io")
      }
    }

    "find classes of specific types" in {
      val l = Resource.findClasses("scala.io", classOf[scala.io.Source])
      l.size should be > 0
      debug(l)
      for(each <- l) {
        debug(each)
        classOf[scala.io.Source].isAssignableFrom(each) should be (true)
      }

    }

  }
}