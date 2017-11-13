import sbt._
import Keys._

object build extends Build {
  val laws = project
  val tests = project dependsOn laws
}
