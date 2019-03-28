// Intended for use with publishLocal version Scala and
// locally built and publishLocal'ed sourcecode
//
// Scala build--just `sbt publishLocal`
// sourcecode build--edit build.sbt to have scala213 be the default
//   that refers to 2.13.0-pre-SNAPSHOT (for now)
//   and then do project sourcecodeJVM and publishLocal

version in ThisBuild := "0.6.0"

scalaVersion in ThisBuild := "2.13.0-pre-bd28253"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-feature", "-deprecation")

libraryDependencies in ThisBuild ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.lihaoyi" %% "sourcecode" % "0.1.5+9-d989dc50-SNAPSHOT"
)


val laws = project

val tests = project dependsOn laws
