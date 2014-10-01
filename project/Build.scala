import sbt._
import Keys._

object build extends Build {
  val laws = project
  val inst = project dependsOn laws
  val tests = project dependsOn (laws, inst)
}


