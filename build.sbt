name := "genetic-machine"

version := "1.0"

scalaVersion := "2.11.2"

scalacOptions += "-feature"

scalacOptions += "-deprecation"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test"

// libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.6"

// libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.3.6" % "test"

// libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.6"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.10"

// libraryDependencies += "org.scalanlp" %% "breeze-natives" % "0.10"

libraryDependencies += "org.neo4j" % "neo4j" % "2.1.4"

//libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.9.0"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.9"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.13"
