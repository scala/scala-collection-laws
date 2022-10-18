ThisBuild / scalaVersion := "2.13.10"
ThisBuild / scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")
ThisBuild / libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.lihaoyi" %% "sourcecode" % "0.3.0"
)

val laws = project.settings(
  scalacOptions += "-Werror",
  scalacOptions += "-Xsource:3",
)
val tests = project.dependsOn(laws)
