/*
 * Copyright 2012 Taro L. Saito
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package xerial

import java.io.File
import sbt._
import Keys._

import sbtrelease.ReleasePlugin._
import com.jsuereth.pgp.sbtplugin.PgpPlugin._

object XerialBuild extends Build {

  val SCALA_VERSION = "2.9.2"

  def releaseResolver(v: String): Resolver = {
    val profile = System.getProperty("xerial.profile", "default")
    profile match {
      case "default" => {
        val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT"))
          "snapshots" at nexus + "content/repositories/snapshots"
        else
          "releases" at nexus + "service/local/staging/deploy/maven2"
      }
      case p => {
        sys.error("unknown xerial.profile:%s".format(p))
      }
    }
  }

  lazy val buildSettings = Defaults.defaultSettings ++ Unidoc.settings ++ releaseSettings ++ Seq[Setting[_]](
    organization := "org.xerial",
    organizationName := "Xerial Project",
    organizationHomepage := Some(new URL("http://xerial.org/")),
    description := "Xerial: Data Management Utiilities",
    scalaVersion := SCALA_VERSION,
    resolvers <++= version { (v) =>
        Seq("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases", releaseResolver(v))
    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishTo <<= version { (v) => Some(releaseResolver(v)) },
    pomIncludeRepository := {
      _ => false
    },
    parallelExecution := true,
    crossPaths := false,
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-target:jvm-1.5"),
    pomExtra := {
      <url>http://xerial.org/</url>
      <licenses>
        <license>
          <name>Apache 2</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        </license>
      </licenses>
        <scm>
          <connection>scm:git:github.com/xerial/xerial.git</connection>
          <developerConnection>scm:git:git@github.com:xerial/xerial.git</developerConnection>
          <url>github.com/xerial/xerial.git</url>
        </scm>
        <properties>
          <scala.version>
            {SCALA_VERSION}
          </scala.version>
          <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        </properties>
        <developers>
          <developer>
            <id>leo</id>
            <name>Taro L. Saito</name>
            <url>http://xerial.org/leo</url>
          </developer>
        </developers>
    },
    useGpg := true,
    useGpgAgent := false
  )


  import Dist._
  import Dependencies._

  private val dependentScope = "test->test;compile->compile"

  // Project modules
  lazy val root = Project(
    id = "xerial",
    base = file("."),
    settings = buildSettings ++ distSettings ++ Seq(packageDistTask) ++
      Seq(libraryDependencies ++= bootLib)
  ) aggregate(core, lens, cui, clio)

  lazy val core = Project(
    id = "xerial-core",
    base = file("xerial-core"),
    settings = buildSettings ++ Seq(
      description := "Xerial core utiltiles",
      libraryDependencies ++= testLib
    )
  )

  lazy val lens = Project(
    id = "xerial-lens",
    base = file("xerial-lens"),
    settings = buildSettings ++ Seq(
      description := "Utilities for retrieving object type information",
      libraryDependencies ++= testLib ++ lensLib
    )
  ) dependsOn (core % dependentScope)

  lazy val cui = Project(
    id = "xerial-cui",
    base = file("xerial-cui"),
    settings = buildSettings ++ Seq(
      description := "command line parser and launcher",
      libraryDependencies ++= testLib
    )
  ) dependsOn(lens % dependentScope)

  lazy val clio = Project(
   id = "xerial-clio",
   base = file("xerial-clio"),
    settings = buildSettings ++ Seq(
     description := "cluster resource management platform",
     libraryDependencies ++= clioLib
   )
  ) dependsOn(lens % dependentScope)



  object Dependencies {
    val testLib = Seq(
      "org.scalatest" %% "scalatest" % "2.0.M1" % "test"
    )

    val bootLib = Seq(
      "org.codehaus.plexus" % "plexus-classworlds" % "2.4" % "provided"
    )

    val lensLib = Seq(
      "org.javassist" % "javassist" % "3.15.0-GA",
      "org.scala-lang" % "scalap" % SCALA_VERSION
    )
    val clioLib = Seq(
      "org.apache.zookeeper" % "zookeeper" % "3.4.3" excludeAll(
        ExclusionRule(organization="com.sun.jdmk"),
        ExclusionRule(organization="com.sun.jmx"),
        ExclusionRule(organization="javax.jms")),
      "io.netty" % "netty" % "3.5.7.Final",
      "org.xerial.snappy" % "snappy-java" % "1.0.5-M3",
      "com.netflix.curator" % "curator-recipes" % "1.2.3",
      "com.netflix.curator" % "curator-test" % "1.2.3" % "test"
    )
  }

}








