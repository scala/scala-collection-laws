name := "collections-laws"

version := "0.5.0"

scalaVersion := "2.13.0-M2"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
// libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.3"
scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")
