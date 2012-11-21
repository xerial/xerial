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

import sbt._
import Keys._

import sbtrelease.ReleasePlugin._

object XerialBuild extends Build {

  val SCALA_VERSION = "2.9.2"

  private def profile = System.getProperty("xerial.profile", "default")
  private def isWindows = System.getProperty("os.name").contains("Windows")

  def releaseResolver(v: String): Option[Resolver] = {
    profile match {
      case "default" => {
        val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases" at nexus + "service/local/staging/deploy/maven2")
      }
      case p => {
        scala.Console.err.println("unknown xerial.profile:%s".format(p))
        None
      }
    }
  }


  lazy val defaultJavacOptions = Seq("-encoding", "UTF-8", "-deprecation", "-source", "1.5", "-target", "1.5")
  lazy val defaultScalacOptions = Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-target:jvm-1.5")

  lazy val buildSettings = Defaults.defaultSettings ++ Unidoc.settings ++ releaseSettings ++ Seq[Setting[_]](
    organization := "org.xerial",
    organizationName := "Xerial Project",
    organizationHomepage := Some(new URL("http://xerial.org/")),
    description := "Xerial: Data Management Utiilities",
    scalaVersion := SCALA_VERSION,
//    resolvers <++= version { (v) =>
//        Seq("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases", releaseResolver(v))
//    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishTo <<= version { (v) => releaseResolver(v) },
    pomIncludeRepository := {
      _ => false
    },
    parallelExecution := true,
    crossPaths := false,
    javacOptions := defaultJavacOptions,
    scalacOptions in Compile := defaultScalacOptions,
    scalacOptions in doc <++= (baseDirectory in LocalProject("xerial"), version) map { (bd, v) =>
      val tree = if(v.endsWith("-SNAPSHOT")) "develop" else "master"
      defaultScalacOptions ++ Seq(
        "-sourcepath", bd.getAbsolutePath,
        "-doc-source-url", "http://github.com/xerial/xerial/blob/" + tree + "€{FILE_PATH}.scala",
        "-doc-title", "Xerial",
        "-doc-version", v
      )
    },
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
    }
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
  ) aggregate(core, lens)

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
      description := "Object mapping utiltiles",
      libraryDependencies ++= testLib ++ lensLib
    )
  ) dependsOn (core % dependentScope)

  object Dependencies {
    val testLib = Seq(
      "org.scalatest" %% "scalatest" % "2.0.M5" % "test"
    )

    val bootLib = Seq(
      "org.codehaus.plexus" % "plexus-classworlds" % "2.4" % "provided"
    )

    val lensLib = Seq(
      "org.javassist" % "javassist" % "3.15.0-GA",
      "org.scala-lang" % "scalap" % SCALA_VERSION
    )
  }

}








