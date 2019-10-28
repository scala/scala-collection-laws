version in ThisBuild := "0.6.0"

scalaVersion in ThisBuild := "2.13.1"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-feature", "-deprecation")

libraryDependencies in ThisBuild ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.lihaoyi" %% "sourcecode" % "0.1.8"
)

val laws = project

val tests = project.dependsOn(laws)
