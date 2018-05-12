name := "collections-laws"

version := "0.6.0"

scalaVersion := "2.13.0-pre-SNAPSHOT"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.5-local-SNAPSHOT"
scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")
