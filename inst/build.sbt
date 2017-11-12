scalaVersion := "2.12.4"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.3"
libraryDependencies += "com.novocode" % "junit-interface" % "0.9"
scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")
