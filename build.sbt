import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._
import com.typesafe.sbt.packager.archetypes.ServerLoader.{SystemV, Upstart}

name := "genetic-machine"

version := "1.0"

packageArchetype.java_server

maintainer in Linux := "Maxim Borisyak <Maxim.Borisyak at google.com>"

packageSummary in Linux := "Genetic + Machine"

packageDescription in Linux := "A long description of Genetic + Machine"

rpmVendor := "MIPT"

scalaVersion := "2.11.1"

scalacOptions += "-feature"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"


libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.5" % "test"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.4",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.4" % "test",
  "com.typesafe.akka" %% "akka-remote" % "2.3.4"
)

libraryDependencies  ++= Seq(
  "org.scalanlp" % "breeze_2.10" % "0.7",
  "org.scalanlp" % "breeze-natives_2.10" % "0.7"
)

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies += "org.neo4j" % "neo4j" % "2.1.2"

libraryDependencies += "org.scala-lang" % "scala-pickling_2.11" % "0.8.0"