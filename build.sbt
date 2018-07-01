version in ThisBuild := "0.6.0"

scalaVersion in ThisBuild := "2.13.0-M4"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-feature", "-deprecation")

libraryDependencies in ThisBuild ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.lihaoyi" %% "sourcecode" % "0.1.5-SNAPSHOT")


val laws = project

val tests = project dependsOn laws
