
addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.5")

addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.6.5")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.2.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3")

scalacOptions ++= Seq("-deprecation", "-feature")
