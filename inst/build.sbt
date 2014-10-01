def genInsts(cp: Classpath, out: File, main: Option[String], run: ScalaRun, s: TaskStreams): Seq[File] = {
  // IO.delete(out)
  if (!out.exists) IO.createDirectory(out)
  val args = "--Instances" :: out.getAbsolutePath :: Nil
  val mainClass = main getOrElse "No main class defined for Instances generator"
  toError(run.run(mainClass, cp.files, args, s.log))
  (out ** "*.scala").get
}

(sourceGenerators in Compile) <+= (
  fullClasspath in Compile in laws,
  sourceManaged in Compile,
  mainClass in Compile in laws,
  runner, streams
) map (genInsts _)
