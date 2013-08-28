addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.7.1")

addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.3.0")

scalacOptions ++= Seq("-deprecation", "-feature")
