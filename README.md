xerial
===========

# xerial-core
Core utilities for xerial projects.

 * Logger
 * Command line to object mapper
 * StopWatch for measuring code performance
 * Resource for reading files in classpath and jar files
 * Fast PEG parser generator 

# xerial-lens
Retrives object type information

 * ObjectSchema for getting full-fledged type information including generic types
 * Eq trait for injecting field-value based hashCode and equals method to any objects
 * Command-line paraser that can call methods in a class by mapping command line arguments to method arguments by using the information of ObjectSchema

## Release plan

 * Version 3.0: Scala-based release. 
  * Migrating common utilities from Java to Scala

## Usage
Add the following settings to your sbt build file (e.g., `project/build.sbt`)

    resolvers += "xerial snapshot"  at "http://maven.xerial.org/repository/snapshot/"
    
    libraryDependencies += "org.xerial" % "xerial-core" % "3.0-SNAPSHOT"

