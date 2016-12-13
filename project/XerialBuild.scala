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


import sbt._
import sbt.Keys._
import xerial.sbt.Pack._

object XerialBuild extends Build {

  lazy val buildSettings = Defaults.coreDefaultSettings ++ Seq[Setting[_]](
    organization := "org.xerial",
    organizationName := "Xerial Project",
    organizationHomepage := Some(new URL("http://xerial.org/")),
    scalaVersion := "2.12.1",
    description := "Xerial: Data Management Utilities",
    publishArtifact in Test := false,
    pomIncludeRepository := {
      _ => false
    },
    concurrentRestrictions in Global := Seq(
      Tags.limit(Tags.Test, 1)
    ),
    logBuffered in Test := false,
    // Since sbt-0.13.2
    incOptions := incOptions.value.withNameHashing(true),
    crossPaths := true,
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-feature"),
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
            {scalaVersion.value}
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

  import Dependencies._

  private val dependentScope = "test->test;compile->compile"

  // Project modules
  lazy val root = Project(
    id = "xerial",
    base = file("."),
    settings = buildSettings ++ packSettings ++ Seq(
      crossScalaVersions := Seq("2.12.1", "2.11.8", "2.10.6"),
      packExclude := Seq("root"),
      packMain := Map("xerial" -> "xerial.lens.cui.Main"),
      publishArtifact := false
    )
  ) aggregate(core, lens, compress)

  lazy val core = Project(
    id = "xerial-core",
    base = file("xerial-core"),
    settings = buildSettings ++ Seq(
      description := "Xerial core utiltiles",
      crossScalaVersions := Seq("2.12.1", "2.11.8", "2.10.6"),
      libraryDependencies ++= testLib ++ (scalaVersion.value match {
        case "2.10.6" => Seq.empty
        case _ => Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4")
      })
    )
  )

  def scalaCompilerDependency(scalaVersion: String) = Seq(
    "org.scala-lang" % "scalap" % scalaVersion,
    "org.scala-lang" % "scala-reflect" % scalaVersion
  )

  lazy val lens = Project(
    id = "xerial-lens",
    base = file("xerial-lens"),
    settings = buildSettings ++ Seq(
      description := "Object mapping utiltiles",
      crossScalaVersions := Seq("2.12.1", "2.11.8"),
      libraryDependencies ++= testLib ++ scalaCompilerDependency(scalaVersion.value) ++ Seq(
        "org.javassist" % "javassist" % "3.19.0-GA"
      )
    )
  ) dependsOn (core % dependentScope)

  lazy val compress = Project(
    id = "xerial-compress",
    base = file("xerial-compress"),
    settings = buildSettings ++ Seq(
      description := "Compression libraries",
      crossScalaVersions := Seq("2.12.1", "2.11.8", "2.10.6"),
      libraryDependencies ++= testLib ++ Seq(
        "org.xerial.snappy" % "snappy-java" % "1.1.2.1"
      )
    )
  ) dependsOn (core % dependentScope)

//  lazy val macroLib = Project(
//    id = "xerial-macro",
//    base = file("xerial-macro"),
//    settings = buildSettings ++ Seq(
//      description := "macro libraries for Xerial projects",
//      libraryDependencies += "org.scala-lang" % "scala-reflect" % SCALA_VERSION
//    )
//  ) dependsOn (core % dependentScope)


  object Dependencies {
    val testLib = Seq(
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    )
  }

}








