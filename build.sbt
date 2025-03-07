ThisBuild / scalaVersion := "2.13.16"
ThisBuild / scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")
ThisBuild / libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.lihaoyi" %% "sourcecode" % "0.4.2"
)

val laws = project.settings(
  scalacOptions += "-Werror",
  scalacOptions += "-Xsource:3-cross",
)
val tests = project.dependsOn(laws)
