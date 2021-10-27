name := "collections-laws-tests"

libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2"

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
