name := "RepeatedHamming74"

version := "1.0"

scalaVersion := "2.10.2"

libraryDependencies += "org.scalanlp" % "breeze_2.10" % "0.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/releases/"
)