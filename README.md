Xerial Project
===========

The ulitimate goal of Xerial project is to manage everything as database. 

# Modules

## xerial-core
Core utilities of xerial projects.
 
 * Useful collection classes
     * CyclicArray (double-ended queue), RedBlackTree, balanced PrioritySearchTree (*O(log N+k)* for interval-intersection queries), UnionFindSet etc.
 * Logger whose log levels and output targets can be configured through a JMX interface at runtime
     * For use, simply extend `Logging` trait, then call trace, debug, info, warn, error, fatal methods to output logs.
     * Global log levels can be configured through JVM argument (e.g, -Dloglevel=debug) 
 * StopWatch for taking benchmarks of code blocks
     * Repetitive execution of codes is supported.
 * Resource trait for reading files in classpaths and jar files. 
    * Quite useful for writing codes that need to use resource files. (e.g., test data, graphic data, font files, etc.)
 * Fast PEG parser generator
    * (on-going) Producing [Silk format](http://xerial.org/silk) parser codes for serval programming language including Scala(Java), C, etc.
  

## xerial-lens
Retrives object type information embeded in Scala-generated class files. 

 * ObjectSchema for getting full-fledged type information including generic types. 
    * Now you are free-from the type erasure problem!
    * Use `ObjectSchema(cl:Class[_])` to obtain consturctors, methods and the other parameters defined in a class.  
    * SigParser of the scalap is used for reading ScalaSignatures.
 * Eq trait for injecting field-value based hashCode and equals method to any objects
    * Your classes extending Eq trait become ready to use in containers, e.g, Set[K], Map[K, V] etc.  
 * Command-line paraser 
   * You can call methods in a class by mapping command line arguments to the method arguments
   * String values are automatically converted to appropriate data types according to the information obtained by ObjectSchema

# Release plan

 * Version 3.0: Scala-based release. 
  * Migrating common utilities from Java to Scala

## Usage
Add the following settings to your sbt build file (e.g., `project/build.sbt`)

    resolvers += "Sonatype snapshot repo" at "https://oss.sonatype.org/content/repositories/snapshots/"
    
    libraryDependencies += "org.xerial" % "xerial-core" % "3.0-SNAPSHOT"
    
    # When you want to use ObjectSchema
    libraryDependencies += "org.xerial" % "xerial-lens" % "3.0-SNAPSHOT"

