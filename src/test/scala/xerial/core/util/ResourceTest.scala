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
      val cl = Thread.currentThread().getContextClassLoader
      debug(cl)
      val r = cl.getResources("xerial/core/util")
      debug(r.mkString(","))

      debug("find files from package")
      val l = Resource.listResources("xerial.core.util", { s : String => s.endsWith(".class")})
      l.size should be > 0
    }

    "find resources from jar files" in {
      debug("find files from a jar file")

      val l = Resource.listResources("scala.io")
      l.size should be > 0
    }

  }
}