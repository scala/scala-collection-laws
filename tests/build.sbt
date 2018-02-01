name := "collections-laws-tests"

version := "0.5.0"

scalaVersion := "2.13.0-M2"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
// libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.3"
libraryDependencies += "com.novocode" % "junit-interface" % "0.9"

unmanagedJars in Compile += file("../laws/target/scala-2.13.0-M2/collections-laws_2.13.0-M2-0.5.0.jar")
unmanagedJars in Test    += file("../laws/target/scala-2.13.0-M2/collections-laws_2.13.0-M2-0.5.0.jar")

/*
// Old version.  Needs to be tweaked to work.
(sourceGenerators in Compile) += Def.task {
  val out = (sourceManaged in Compile).value
  if (!out.exists) IO.createDirectory(out)
  val args = out.getAbsolutePath :: ("--versionID=" + (scalaVersion in Compile).value) :: Nil
  val runTarget = (mainClass in Compile in inst).value getOrElse "No main class defined for tests generator"
  val classPath = (fullClasspath in Compile in inst).value
  toError(runner.value.run(runTarget, classPath.files, args, streams.value.log))
  (out ** "*.scala").get
}.taskValue
*/