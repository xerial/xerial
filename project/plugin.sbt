
addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.7.1")

addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.5.1")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.2.1")

scalacOptions ++= Seq("-deprecation", "-feature")
