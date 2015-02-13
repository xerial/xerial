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


import java.io.File
import sbt._
import sbt.Keys._
import sbtrelease.ReleasePlugin._
import xerial.sbt.Pack._
import xerial.sbt.Sonatype._

object XerialBuild extends Build {

  val SCALA_VERSION = "2.11.5"

  lazy val buildSettings = Defaults.coreDefaultSettings ++ releaseSettings ++ Seq[Setting[_]](
    organization := "org.xerial",
    organizationName := "Xerial Project",
    organizationHomepage := Some(new URL("http://xerial.org/")),
    description := "Xerial: Data Management Utilities",
    scalaVersion in Global := SCALA_VERSION,
    publishArtifact in Test := false,
    testOptions in Test <+= (target in Test) map {
      t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")".format(t /"test-reports" ), "stdout")
    },
    publishMavenStyle := true,
    pomIncludeRepository := {
      _ => false
    },
    concurrentRestrictions in Global := Seq(
      Tags.limit(Tags.Test, 1)
    ),
    ReleaseKeys.tagName := {version.value},
    // Since sbt-0.13.2
    incOptions := incOptions.value.withNameHashing(true),
    crossPaths := false,
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-target:jvm-1.6", "-feature"),
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
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


  import Dependencies._

  private val dependentScope = "test->test;compile->compile"

  // Project modules
  lazy val root = Project(
    id = "xerial",
    base = file("."),
    settings = buildSettings ++ packSettings ++ Seq(
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
      libraryDependencies ++= testLib ++ coreLib
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

  lazy val compress = Project(
    id = "xerial-compress",
    base = file("xerial-compress"),
    settings = buildSettings ++ Seq(
      description := "Compression libraries",
      libraryDependencies ++= testLib ++ Seq(
        "org.xerial.snappy" % "snappy-java" % "1.1.1.6"
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
      "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
    )

    val coreLib = Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
    )

    val lensLib = Seq(
      "org.javassist" % "javassist" % "3.15.0-GA",
      "org.scala-lang" % "scalap" % SCALA_VERSION,
      "org.scala-lang" % "scala-reflect" % SCALA_VERSION
    )
  }

}








