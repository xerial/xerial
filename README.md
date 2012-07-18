xerial-core
===========

Core utilities for xerial projects.

## Contents
 * Logger
 * Command line to object mapper
 * StopWatch for measuring code performance
 * Resource for reading files in classpath and jar files
 * ObjectSchema for getting full-fledged type information including generic types 

## Release plan

 * Version 3.0: Scala-based release. 
  * Migrating common utilities from Java to Scala

## Usage
Add the following settings to your sbt build file (e.g., `project/build.sbt`)

    resolvers += "xerial snapshot"  at "http://maven.xerial.org/repository/snapshot/"
    
    libraryDependencies += "org.xerial" % "xerial-core" % "3.0-SNAPSHOT"

