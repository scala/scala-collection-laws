scalaVersion := "2.12.2"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.3"
scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")
