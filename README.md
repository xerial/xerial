Xerial Project
===========

Xerial is data manegment utilties for Scala. 
The ulitimate goal of Xerial project is to manage everything as database,
including class objects, text format data (json, XML, Silk, etc.), data
streams, etc.

# Modules

## xerial-core
Core utilities of xerial projects. No dependencies other than the
scala-library exists in xerial-core.
 
 * Useful collection classes
     * CyclicArray (double-ended queue), RedBlackTree, balanced PrioritySearchTree (*O(log N+k)* for interval-intersection queries), UnionFindSet etc.
 * Logger whose log levels and output targets can be configured through a JMX interface at runtime
     * For use, simply extend `xerial.core.log.Logger` trait in your class, then call trace, debug, info, warn, error, fatal methods to output logs.
     * Global log levels can be configured through JVM argument (e.g, -Dloglevel=debug) 
 * Better benchmarking with Timer trait
     * Extend `xerial.core.util.Timer` trait, then wrap your code with `time`
 method. The execution time of the wrapped code will be reported (in debug log)
     * You can also divide your code into sub blocks with `block` method.
     * Repetitive execution is supported; Use `time(repeat=(Int))` or `block(repeat=(Int))`.
 * Resource trait for reading files in classpaths and jar files. 
    * Quite useful for reading resource files. (e.g., test data, graphic data, font files, etc.)
 * Fast PEG parser generator
    * (on-going) Producing [Silk format](http://xerial.org/silk) parser codes for serval programming language including Scala(Java), C, etc.
  
## xerial-lens
Retrives object type information embeded in Scala-generated class files. 

 * ObjectSchema for getting full-fledged type information including generic types. 
    * Now you are free-from the type erasure problem!
    * Use `ObjectSchema(cl:Class[_])` to obtain consturctors, methods and the other parameters defined in a class.  
    * SigParser of the scalap is used for reading ScalaSignatures.

### Applications of ObjectSchema
 * Eq trait for injecting field-value based hashCode and equals method to any objects
    * Your classes extending Eq trait become ready to use in containers, e.g, Set[K], Map[K, V] etc.  

 * Command-line parser (xerial-cui)
   * You can call methods in a class by mapping command line arguments to the method arguments
   * String values are automatically converted to appropriate data types according to the information obtained by ObjectSchema

# Release plan

 * Version 3.0: Scala-based release. 
  * Migrating common utilities from Java to Scala

## Usage
Add the following settings to your sbt build file (e.g., `build.sbt`)

    libraryDependencies += "org.xerial" % "xerial-core" % "3.0"
    
    # When you want to use ObjectSchema
    libraryDependencies += "org.xerial" % "xerial-lens" % "3.0"

    # command line parser
    libraryDependencies += "org.xerial" % "xerial-cui" % "3.0"

#### Using Snapshot version

    resolvers += "Sonatype snapshot repo" at "https://oss.sonatype.org/content/repositories/snapshots/"
    
    libraryDependencies += "org.xerial" % "xerial-core" % "3.0.1-SNAPSHOT"


## Scala API

(Unidoc of API will be prepared)

* [xerial-core 3.0 API](https://oss.sonatype.org/service/local/repositories/releases/archive/org/xerial/xerial-core/3.0/xerial-core-3.0-javadoc.jar/!/index.html)
* [xerial-lens 3.0 API](https://oss.sonatype.org/service/local/repositories/releases/archive/org/xerial/xerial-lens/3.0/xerial-lens-3.0-javadoc.jar/!/index.html)
* [xerial-cui 3.0 API](https://oss.sonatype.org/service/local/repositories/releases/archive/org/xerial/xerial-cui/3.0/xerial-cui-3.0-javadoc.jar/!/index.html)

