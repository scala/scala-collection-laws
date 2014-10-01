def genTests(cp: Classpath, out: File, main: Option[String], run: ScalaRun, s: TaskStreams): Seq[File] = {
  // IO.delete(out)
  if (!out.exists) IO.createDirectory(out)
  val args = out.getAbsolutePath :: Nil
  val mainClass = main getOrElse "No main class defined for tests generator"
  toError(run.run(mainClass, cp.files, args, s.log))
  (out ** "*.scala").get
}

(sourceGenerators in Compile) <+= (
  fullClasspath in Compile in inst,
  sourceManaged in Compile,
  mainClass in Compile in inst,
  runner, streams
) map (genTests _)
