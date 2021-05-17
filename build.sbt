ThisBuild / version := "0.6.0"
ThisBuild / scalaVersion := "2.13.6"
ThisBuild / scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")
ThisBuild / libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.lihaoyi" %% "sourcecode" % "0.2.7"
)

val laws = project
val tests = project.dependsOn(laws)
