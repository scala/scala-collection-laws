(sourceGenerators in Compile) += Def.task {
  val out = (sourceManaged in Compile).value
  if (!out.exists) IO.createDirectory(out)
  val args = "--Instances" :: out.getAbsolutePath :: ("--versionID=" + (scalaVersion in Compile).value) :: Nil
  val runTarget = (mainClass in Compile in laws).value getOrElse "No main class defined for tests generator"
  val classPath = (fullClasspath in Compile in laws).value
  toError(runner.value.run(runTarget, classPath.files, args, streams.value.log))
  (out ** "*.scala").get
}.taskValue
