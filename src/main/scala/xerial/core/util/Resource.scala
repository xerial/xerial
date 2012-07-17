package xerial.core.util

import java.io.{File, BufferedInputStream, InputStream}
import java.util.jar.{JarEntry, JarFile}
import java.util.Enumeration
import java.lang.reflect.Modifier
import java.net.{URLClassLoader, URL}

//--------------------------------------
//
// Resource.scala
// Since: 2012/07/17 9:12
//
//--------------------------------------

/**
 * Resource file manager.
 *
 * @author leo
 */
object Resource extends Logging {

  /**
   * Open a resource as a stream, then execute the code block using the stream
   * @param referenceClass context class to specify the package containing the resource file
   * @param resourceFileName file name
   * @param body code block
   * @tparam U
   */
  def open[U](referenceClass: Class[_], resourceFileName: String)(body: BufferedInputStream => U) {
    val u = find(referenceClass, resourceFileName)
    if (u.isEmpty)
      sys.error("Resource %s (in %s) not found".format(resourceFileName, referenceClass.getSimpleName))

    u.map {
      url =>
        val s = new BufferedInputStream(url.openStream())
        try
          body(s)
        finally
          s.close
    }
  }


  private def packagePath(referenceClass: Class[_]): String = {
    return packagePath(referenceClass.getPackage)
  }
  private def packagePath(basePackage: Package): String = {
    return packagePath(basePackage.getName)
  }
  private def packagePath(packageName: String): String = {
    val packageAsPath: String = packageName.replaceAll("\\.", "/")
    return if (packageAsPath.endsWith("/")) packageAsPath else packageAsPath + "/"
  }

  /**
   * @return Stream of class loaders in the path from the specified class loader to the root class loader
   */
  private def classLoaders(cl: ClassLoader): Stream[URLClassLoader] = {
    def stream(c: ClassLoader): Stream[URLClassLoader] = {
      c match {
        case null => Stream.empty
        case u:URLClassLoader => u #:: stream(c.getParent)
        case _ => stream(c.getParent)
      }
    }
    stream(cl)
  }

  /**
   * @return Stream of class loaders in the path from current class loader to the root class loader
   */
  private def classLoaders: Stream[ClassLoader] = classLoaders(Thread.currentThread.getContextClassLoader)


  private def resolveResourcePath(packageName: String, resourceFileName: String) = {
    val path: String = packagePath(packageName)
    val p = path + resourceFileName
    if (!p.startsWith("/"))
      "/" + p
    else
      p
  }

  def find(referenceClass: Class[_], resourceFileName: String): Option[URL] = {
    find(packagePath(referenceClass), resourceFileName)
  }

  /**
   * Find a resource from the give absolute path
   * @param absoluteResourcePath
   * @return
   */
  def find(absoluteResourcePath: String) : Option[URL] =
    find("", if (absoluteResourcePath.startsWith("/")) absoluteResourcePath.substring(1) else absoluteResourcePath)

  /**
   * Finds the [[java.net.URL]] of the resource
   *
   * @param packageName
   *            the base package name to find the resource
   * @param resourceFileName
   *            the resource file name relative to the package folder
   * @return the URL of the specified resource
   */
  def find(packageName: String, resourceFileName: String): Option[URL] = {
    val resourcePath = resolveResourcePath(packageName, resourceFileName)
    trace("search resource: %s", resourcePath)

    val r = classLoaders.map(_.getResource(resourcePath)).
      collectFirst {
      case path: URL => path
    }

    r orElse Option(this.getClass.getResource(resourcePath))
  }

  /**
   * VirtualFile is a common interface to handle system files and file resources in JAR.
   *
   * System file resources have an URL prefixed with "file:".
   *   e.g., "file:/C:/Program Files/Software/classes/org/xerial/util/FileResource.java"
   * JAR file contents have an URL prefixed with "jar:file:
   *   e.g., "jar:file:/C:/Program Files/Software/something.jar!/org/xerial/util/FileResource.java"
   *
   * @author leo
   *
   */
  abstract trait VirtualFile {
    /**
     * Gets the logical path of the file.
     * For example, if this VirtualFile' URL is "file:/somewhere/org/xerial/util/FileResource.java",
     * its logical name is org/xerial/util/FileResource.java, beginning from the root package.
     * @return
     */
    def logicalPath: String
    /**
     * is directory?
     * @return true when the file is a directory, otherwise false
     */
    def isDirectory: Boolean
    /**
     * Gets the URL of this file
     * @return
     */
    def url: URL

  }
  /**
   * A virtual file implementation for usual files
   *
   * @author leo
   *
   */
  case class SystemFile(file: java.io.File, logicalPath: String) extends VirtualFile {
    def url: URL = file.toURI.toURL

    def isDirectory: Boolean = file.isDirectory
    override def toString: String = url.toString
  }

  /**
   * A virtual file implementation for file resources contained in a JAR file
   *
   * @author leo
   *
   */
  case class FileInJar(resourceURL: URL, logicalPath: String, isDirectory: Boolean) extends VirtualFile {
    if (resourceURL == null)
      sys.error("resource URL cannot be null: " + logicalPath)

    def url = resourceURL

    override def toString: String = url.toString
  }

  private def extractLogicalName(packagePath: String, resourcePath: String): String = {
    val p = if (!packagePath.endsWith("/")) packagePath + "/" else packagePath
    val pos: Int = resourcePath.indexOf(p)
    if (pos < 0) return null
    val logicalName: String = resourcePath.substring(pos + p.length)
    return logicalName
  }

  private def collectFileResources(resourceURLString: String, packagePath: String, resourceFilter: String => Boolean): Seq[VirtualFile] = {
    val logicalName = extractLogicalName(packagePath, resourceURLString)
    if (logicalName == null)
      throw new IllegalArgumentException("packagePath=" + packagePath + ", resourceURL=" + resourceURLString)

    val b = Seq.newBuilder[VirtualFile]
    val file: File = new File(new URL(resourceURLString).toURI)
    if (resourceFilter(file.getPath))
      b += SystemFile(file, logicalName)
    if (file.isDirectory) {
      for (childFile <- file.listFiles) {
        val childResourceURL = resourceURLString + (if (resourceURLString.endsWith("/")) "" else "/") + childFile.getName
        collectFileResources(childResourceURL, packagePath, resourceFilter)
      }
    }
    b.result()
  }
  /**
   * Create a list of all resources under the given resourceURL recursively. If the
   * resourceURL is a file, this method searches directories under the path. If the resource is contained
   * in a Jar file, it searches contents of the Jar file.
   *
   * @param resourceURL
   * @param packagePath  package path under consideration
   * @param resourceFilter
   * @return the list of resources matching the given resource filter
   */
  private def listResources(resourceURL: URL, packagePath: String, resourceFilter: String => Boolean): Seq[VirtualFile] = {
    debug("listResource: url=" + resourceURL)

    val fileList = Seq.newBuilder[VirtualFile]
    if (resourceURL == null)
      return Seq.empty

    val protocol = resourceURL.getProtocol
    if (protocol == "file") {
      val resourceURLString = resourceURL.toString
      fileList ++= collectFileResources(resourceURLString, packagePath, resourceFilter)
    }
    else if (protocol == "jar") {
      val path: String = resourceURL.getPath
      val pos: Int = path.indexOf("!")
      if (pos < 0)
        throw new IllegalArgumentException("invalid resource URL: " + resourceURL)

      val jarPath = path.substring(0, pos) replaceAll("%20", " ") replace("file:", "")
      val jarURLString = "jar:" + jarPath
      val jf: JarFile = new JarFile(jarPath)
      val entryEnum: Enumeration[JarEntry] = jf.entries
      while (entryEnum.hasMoreElements) {
        val jarEntry = entryEnum.nextElement
        val physicalURL = jarURLString + "!/" + jarEntry.getName
        val jarFileURL = new URL(physicalURL)
        val logicalName = extractLogicalName(packagePath, jarEntry.getName)
        if (logicalName != null && resourceFilter(logicalName))
          fileList += FileInJar(jarFileURL, logicalName, jarEntry.isDirectory)
      }
    }
    else {
      throw new UnsupportedOperationException("resources other than file or jar are not supported: " + resourceURL)
    }

    fileList.result
  }

  /**
   * Collect resources under the given package
   * @param packageName
   * @return
   */
  def listResources(packageName:String) : Seq[VirtualFile] =
    listResources(packageName, {f:String => true})


  /**
   * Collect resources under the given package
   * @param classLoader
   * @param packageName
   * @param resourceFilter
   * @return
   */
  def listResources(packageName: String, resourceFilter: String => Boolean, classLoader: ClassLoader = Thread.currentThread.getContextClassLoader): Seq[VirtualFile] = {
    val b = Seq.newBuilder[VirtualFile]
    for (u <- findResourceURLs(classLoader, packageName)) {
      b ++= listResources(u, packageName, resourceFilter)
    }
    b.result
  }

  /**
   * Find resource URLs that can be found from a given class loader and its ancestors
   * @param cl  class loader
   * @param name resource name
   * @return
   */
  def findResourceURLs(cl: ClassLoader, name: String): Seq[URL] = {
    debug("find resource URLs: %s", name)
    val b = Seq.newBuilder[URL]
    for (c: URLClassLoader <- classLoaders(cl)) {
      val e = c.findResources(name)
      while (e.hasMoreElements)
        b += e.nextElement
    }

    b.result
  }



  def findClasses[A](searchPath: Package, toSearch: Class[A], classLoader: ClassLoader = Thread.currentThread.getContextClassLoader): Seq[Class[A]] = {
    val packageName = searchPath.getName
    val classFileList = listResources(packageName, { f:String => f.endsWith(".class") }, classLoader)
    
    def componentName(path:String) : Option[String] = {
      val dot: Int = path.lastIndexOf(".")
      if (dot <= 0)
        None
      else
        Some(path.substring(0, dot).replaceAll("/", "."))
    } 
    def findClass(name:String) : Option[Class[_]] = {
      try
        Some(Class.forName(name, false, classLoader))
      catch {
        case e: ClassNotFoundException => None
      }
    }

    val b = Seq.newBuilder[Class[A]]
    for (vf <- classFileList; cn <- componentName(vf.logicalPath)) {
      val className: String = packageName + "." + cn
      for(cl <- findClass(className)) {
        if (!Modifier.isAbstract(cl.getModifiers) && toSearch.isAssignableFrom(cl)) {
          b += cl.asInstanceOf[Class[A]]
        }
      }
    }
    b.result
  }
}