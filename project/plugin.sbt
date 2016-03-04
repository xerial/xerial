addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.2")

addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.7.9")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "1.1")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("com.eed3si9n" % "sbt-doge" % "0.1.5")

scalacOptions ++= Seq("-deprecation", "-feature")
