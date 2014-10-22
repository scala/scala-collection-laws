def genInsts(cp: Classpath, out: File, main: Option[String], run: ScalaRun, s: TaskStreams, ver: String): Seq[File] = {
  // IO.delete(out)
  if (!out.exists) IO.createDirectory(out)
  val args = "--Instances" :: out.getAbsolutePath :: ("--versionID="+ver) :: Nil
  val mainClass = main getOrElse "No main class defined for Instances generator"
  toError(run.run(mainClass, cp.files, args, s.log))
  (out ** "*.scala").get
}

(sourceGenerators in Compile) <+= (
  fullClasspath in Compile in laws,
  sourceManaged in Compile,
  mainClass in Compile in laws,
  runner,
  streams,
  scalaVersion in Compile
) map (genInsts _)


