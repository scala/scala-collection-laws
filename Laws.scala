package laws

import scala.language.implicitConversions
import scala.language.higherKinds

import scala.util._
import laws.Parsing._

/** Generates and runs single-line collections tests that may be viewed as laws that collections should obey. */
class Laws(junit: Boolean, replacementsFilename: String, linetestsFilename: String, deflag: Set[String], scalaEnv: Laws.ScalaEnv) {
  import Laws._
  
  /** A method generated for testing. */
  case class TestMethod(name: String, code: Vector[String], lines: Set[Int])
  
  /** Given a set of tests that use identical variables and a set of replacements to fill in
    * for variables in those tests, generate a method that runs all the tests (or error messages
    * describing what went wrong)
    */
  def synthMethod(tests: Tests, replaces: Replacements): Either[Vector[String], TestMethod] = {
  
    // Check to make sure all the variables exist in our replacements
    tests.params.filter(x => !replaces.params.contains(x)) match {
      case missing if missing.nonEmpty => return Left(Vector(
        s"Variables ${missing.toList.sorted.mkString(", ")} missing.",
        s"  Required by test lines ${tests.underlying.map(_.line.index).mkString(", ")}",
        s"  Not provided by collection block starting on line ${replaces.myLine}"
      ))
      case _ =>
    }
    
    // Perform all replacements on the test code (bail out with messages if anything goes wrong)
    val replaced = tests.underlying.map(t => replaces(t.code).right.map(_ -> t.line))
    val broken = replaced.collect{ case Left(x) => x }
    if (broken.nonEmpty) return Left(broken)
    
    // Generate body of innermost loop, which is all the test lines properly gathered into assert and with message
    val messageLine = 
      "val message = s\"" +
      tests.params.toList.sorted.map(x => s"$x = $$$x").mkString("; ") + 
      "\" + blockIdentifierString"
    val body = messageLine +: replaced.collect{ case Right((codes, line)) => 
      codes.map{ code =>"if (!{" + code + "}) { throw new AssertionError(\"Test line " + line.index + " with \"+message) }" }
    }.flatten
    
    // Generate code to inject before, inside, and comprehensions for for
    val (pres, fors, ins) = {
      // Use builders since we want to gather across whole set of parameters
      val pb, fb, ib = Vector.newBuilder[String]
      tests.params.toList.sorted.map(replaces.params).map(_.value).foreach{ v =>
        preMacro.macroOn(v, "", s => { pb += s; "" })
        forMacro.macroOn(v, "", s => { fb += s; "" })
        inMacro.macroOn(v, "", s => { ib += s; "" })
      }
      (pb.result, fb.result, ib.result)
    }
    
    // Place injected code in single string so macro expansion will duplicate it if need be
    val single = List(pres.mkString("\n"), fors.mkString("for (", "; ", ") {"), ins.mkString("\n")).mkString("\r")
    
    // Actually perform macro expansion on this string, bailing out with error messages as needed
    val expanded = replaces(single) match {
      case Left(error) =>
        return Left(Vector(
          "Problem with block starting on line ${replaces.myLine} expanding into test with vars ${tests.params.toList.sorted.mkString(", ")}",
          error
        ))
      case Right(xs) => xs
    }
    
    // Now we have all the code, so we just have to add headers and indentation and stuff
    val whole = Vector.newBuilder[String]
    if (junit) lineMacro.argsOf(replaces.infos.get("junitMethodPrefix").map(_.values.mkString(" ")).getOrElse("")).foreach(whole += _)
    val methodName = "test_" + tests.params.toList.sorted.mkString("_")
    whole += s"def $methodName() {"
    
    // Now all the innards of the method
    // Slightly ugly because of how we glued the outer/loop/inner bits together with macro expansion
    expanded.map(_.split("\r",-1)).map{ a =>
      val outer = Vector(hoistOn) ++ (if (a(0).trim.isEmpty) Nil else a(0).split("\n").toList) ++ Vector(hoistOff)
      val loop = if (a.length < 1 || a(1) == "for () {") None else Some(a(1))
      val myBody = if (a.length < 2 || a(2).isEmpty) body else a(2).split("\n").toVector ++ body
      val inner = if (loop.isDefined) myBody.map("  " + _) else myBody
      outer ++ loop ++ inner ++ loop.map(_ => "}")
    } match {
      // Simplify generated code if there's only a single expansion
      case Vector(unique) => whole += "  val blockIdentifierString = \"\""; unique.foreach(whole += "  " + _)
      
      // General case will wrap code for each expansion in its own block
      case more => more.zipWithIndex.foreach{ case (x,i) => 
        whole += "  {"
        whole += "    val blockIdentifierString = \" in group " + (i+1) + "\""
        x.foreach(whole += "    " + _)
        whole += "  }"
      }
    }
    whole += "}"
    
    // Method is complete.
    Right(TestMethod(methodName, whole.result, tests.underlying.map(_.line.index).toSet))
  }

  /** A file that performs tests on a single collection */
  case class TestFile(
    title: String, pkg: String, code: Vector[String],
    unvisited: Option[Set[String]], lines: Set[Int],
    written: Boolean = false
  ) {
    lazy val file = new java.io.File("generated-tests/Test_" + title + ".scala")
    lazy val qualified = (pkg match { case "" => pkg; case _ => pkg + "." }) + "Test_" + title
    def write() = copy(written = freshenFile(file, code))
  }
  
  /** Given tests that use different variables and a set of replacements for a single collection,
    * plus a map that lets us look up which methods are available for that collection, generate
    * a test file (or error messages that tell us what went wrong).
    */
  def synthFile(
    testses: Vector[Tests], replaces: Replacements,
    oracle: Map[String, Set[String]]
  ): Either[Vector[String], TestFile] = {
    
    // Find what methods are available (bailing out if there are errors)
    val myMethods = replaces("@NAME") match {
      case Right(Vector(name)) => oracle.get(name) match {
        case Some(m) => replaces.infos.get("doNotVerifyMethods").map(m ++ _.values.toSet).getOrElse(m)
        case _ => return Left(Vector(s"Cannot find methods for collection named $name"))
      }
      case _ => return Left(Vector(s"Collection name lookup failed for block at ${replaces.myLine}"))
    }
    
    // We also want to know which methods were tested, so find ones needing tests here
    val unvisited = new collection.mutable.HashSet[String]
    unvisited ++= myMethods
    replaces.infos.get("doNotVerifyMethods").foreach(unvisited --= _.values)
    
    // Flags in the replacements specify which tests to run
    val flags = replaces.params.get("flags").map(_.value).getOrElse("").split("\\s+").filter(_.nonEmpty).toSet
    
    // Cut out tests that aren't supposed to run based on flags and which methods are available
    // (secretly keep track of which were requested to be used at the same time)
    val pruned = testses.map(_.filter(x => x.validateFlags(flags) && x.validateMethods(myMethods, unvisited))).filter(_.underlying.nonEmpty) match {
      case Vector() => return Left(Vector(s"No tests match flags set for block starting at line ${replaces.myLine}"))
      case x => x
    }
    
    // Generate all the methods (or bail on errors)
    val (methodCode, hoistables) = {
      val hoistableBuilder = Vector.newBuilder[String]
      val temp = pruned.map(t => synthMethod(t, replaces))
      temp.collect{ case Left(errors) => errors }.reduceOption(_ ++ _) match {
        case Some(errors) => return Left(errors)
        case _ =>
      }
      val lines = temp.collect{ case Right(xs) =>
        var hoisting = false
        val i = xs.code.iterator
        while (i.hasNext) i.next match {
          case x if x endsWith hoistOn  => hoisting = true; println("Hoisting on")
          case x if x endsWith hoistOff => hoisting = false; println("Hoisting off")
          case x => if (hoisting) hoistableBuilder += x
        }
        xs
      }
      (lines, hoistableBuilder.result)
    }
    
    val hoists = makeHoists(hoistables)
    val methods = methodCode.map(c => c.copy(code = hoistOut(c.code, hoists)))
    
    var title = "unknown"   // Hack to pass back collection name info (non-hack is awkward, sadly)
    
    // We have most pieces now, just need to assemble them into a file
    val whole = Vector.newBuilder[String]
    whole += autoComment
    lineMacro.argsOf(replaces.infos.get("fileHeader").map(_.values.mkString(" ")).getOrElse("")).foreach(whole += _)
    if (junit) lineMacro.argsOf(replaces.infos.get("junitFileHeader").map(_.values.mkString(" ")).getOrElse("")).foreach(whole += _)
    whole += ""
    whole += (replaces("@NAME") match {
      // Order is important here!  Only the single-entry vector case is good.
      case Right(Vector(x)) => title = simple(x); s"object Test_$title {"
      case Right(xs) => return Left("Could not decide between alternative names for block starting at line ${replaces.myLine}" +: xs)
      case Left(error) => return Left(Vector("Could not create name for block starting at line ${replaces.myLine}",error))
    })
    
    // Make the hoists
    whole += ""
    whole += "  object Hoists {"
    hoists.toList.sortBy(_._1).foreach{ case (_,v) => whole += "    " + v }
    whole += "  }"
    
    // Include all the test methods
    methods.foreach{ case TestMethod(_, code, _) => whole += ""; code.foreach(whole += "  " + _) }
    
    // If it's not to be used as jUnit tests, create a main method that runs everything
    if (!junit) {
      whole += ""
      whole += "  def main(args: Array[String]) {"
      whole += "    val tests: Vector[() => Unit] = Vector("
      methods.map("      " + _.name + " _").mkString(",\n").split("\n").foreach(whole += _)
      whole += "    )"
      whole += "    val results = tests.map(f => Try(f()))"
      whole += "    val errors = results.collect{ case scala.util.Failure(t) => t }"
      whole += "    if (errors.nonEmpty) {"
      whole += "      println(errors.length + \" errors!\")"
      whole += "      errors.foreach{ e => println; laws.Laws.explainException(e).take(10).foreach(println); }"
      whole += "      sys.exit(1)"
      whole += "    }"
      whole += "    println(\"All tests passed for " + title + "\")"
      whole += "  }"
    }
    whole += "}"
    
    // Find the package name from the fileHeader info
    val pkg = lineMacro.argsOf(replaces.infos.get("fileHeader").map(_.values.mkString(" ")).getOrElse("")).
      find(_ startsWith "package ").
      map(_.drop(7).dropWhile(_.isWhitespace)).
      getOrElse("")
      
    // We have successfully created a test file.
    Right(TestFile(
      title, pkg, whole.result,
      Some(unvisited.toSet), methods.map(_.lines).reduceOption(_ ++ _).getOrElse(Set())
    ))
  }
  
  /** Generate the contents of a file called Instances.scala that can use reflection to get all the available methods for each class */
  def synthInstances(replaceses: Vector[Replacements]): Either[Vector[String], Vector[String]] = {
    // This whole mess is just because we need various bits of information out of the map and all sorts of stuff can go wrong.
    val needed = replaceses.map{ reps =>
      reps("@NAME") match {
        case Left(e) => Left(Vector(e))
        case Right(Vector(name)) => reps("@CCM") match {
          case Left(e) => Left(Vector(e))
          case Right(Vector(coll)) => reps.params.get("instance").map(i => reps(i.value)) match {
            case None => Left(Vector(s"Need a value for 'instance' in block at ${reps.myLine}"))
            case Some(Left(e)) => Left(Vector(e))
            case Some(Right(Vector(code))) => Right((name, coll, code))
            case Some(Right(x)) => Left(s"Need just one instance in block at ${reps.myLine} but found" +: x): Either[Vector[String], (String, String, String)]
          }
          case Right(x) => Left(s"Need just one collection type for method search for block at ${reps.myLine} but found " +: x): Either[Vector[String], (String, String, String)]
        }
        case Right(x) => Left(s"Need just one name for block at ${reps.myLine} but found" +: x): Either[Vector[String], (String, String, String)]
      }
    } |> { ans =>
      // And this part is to pull out any bits that went wrong (we just noted them before)
      ans.collect{ case Left(v) => v }.reduceOption(_ ++ _) match {
        case Some(v) => return Left(v)
        case _ => ans.collect{ case Right(x) => x }
      }
    }
    
    // We store the collections by assigning a bunch of variables
    val assigned = needed.map{ case (name, coll, code) =>
      val v = "inst_" + simple(name)
      (name, v, s"val $v = (classOf[$coll], $code)")
    }
    
    // Now we just build the file
    val whole = Vector.newBuilder[String]
    whole += autoComment
    whole += "package laws"
    whole += "object Instances {"
    assigned.foreach{ case (_, _, code) => whole += "  " + code }
    
    // The key variable is mapped, which stores the methods found from all those variables
    whole += "  val mapped: Map[String, Set[String]] = Map("
    assigned.foreach{ case (name, v, _) => whole += "    \"" + name + "\" -> laws.MethodFinder(" + v + "._1, " + v + "._2 )," }
    whole += "    \"null\" -> Set()"
    whole += "  )"
    whole += "}"
    
    // We have generated the file.
    Right(whole.result)
  }
  
  /** Generate the Instances.scala file, compile it if necessary, and via reflection load it to get method info */
  def getMethodOracle(replaceses: Vector[Replacements], recompile: Boolean = false): Either[Vector[String], Map[String, Set[String]]] = {
    // Make the code or bail if something went wrong
    val instanceCode = synthInstances(replaceses) match {
      case Left(v) => return Left("Could not create code for Instances.scala:" +: v)
      case Right(v) => v
    }
    
    // Create the file (or bail if something went wrong)
    val target = new java.io.File("Instances.scala")
    val changed = Try{ freshenFile(target, instanceCode) } match {
      case Failure(t) => return Left(s"Could not write file ${target.getCanonicalFile.getPath}" +: explainException(t))
      case Success(b) => b
    }
    
    // If the file's contents changed, we have to recompile
    if (changed || recompile) {
      Try {
        println("Compiling Instances.scala.")
        scala.sys.process.Process(scalaEnv.compile("Instances.scala"), (new java.io.File(".")).getCanonicalFile).!
      } match {
        case Failure(t) => return Left(s"Could not compile Instances.scala" +: explainException(t))
        case Success(exitcode) if (exitcode != 0) => return Left(Vector(s"Failed to compile Instances.scala; exit code $exitcode"))
        case _ =>
      }
    }
    
    // Should have gone okay, so load map with reflection or catch the mistake and say what went wrong
    try {
      val obj = Class.forName("laws.Instances$").getDeclaredConstructors.headOption.map{ c => c.setAccessible(true); c.newInstance() }.get
      val meth = Class.forName("laws.Instances$").getDeclaredMethods.find(_.getName == "mapped").get
      Right(meth.invoke(obj).asInstanceOf[Map[String, Set[String]]])
    }
    catch {
      case t: Throwable => 
        if (!recompile) getMethodOracle(replaceses, true)
        else Left("Could not instantiate Instances:" +: explainException(t))
    }
  }
  
  lazy val loadedTestses = Tests.read(linetestsFilename)
  lazy val loadedReplacementses = Replacements.read(replacementsFilename).right.map{ rs =>
    rs.map{ r => r. // Strip out any flags from command-line that you want to ignore
      params.get("flags").
      map(rp => rp -> rp.value.split("\\s+").toSet).
      filter{ case (_,flags) => (flags & deflag).nonEmpty }.
      map{ case(rp, flags) => 
        val x = flags diff deflag
        r.copy(params = r.params.updated("flags", rp.copy(value = x.mkString(" "))))
      }.
      getOrElse(r)
    }.filter{ r =>
      // If the flag BROKEN exists, omit this collection
      r.params.get("flags").map(_.value.split("\\s+").toSet).filter(_.contains("BROKEN")).isEmpty
    }
  }
  
  /** Loads all the files/info needed and generates the tests, including writing the files if their contents have changed. */
  def generateTests(): Either[Vector[String], Vector[TestFile]] = {
    // Load single-line tests (or bail)
    val testses = loadedTestses match {
      case Left(e) => return Left(s"Could not read tests filename $linetestsFilename" +: e)
      case Right(ts) => ts
    }
    
    // Load replacements for each collection (or bail)
    val replaceses = loadedReplacementses match {
      case Left(e) => return Left(s"Could not read replacements filename $replacementsFilename" +: e)
      case Right(rs) => rs
    }
    
    // Load and possibly compile Instances to get available methods (or bail)
    val oracle = getMethodOracle(replaceses) match {
      case Left(e) => return Left(s"Could not acquire available methods:" +: e)
      case Right(ms) => ms
    }
    
    // Create contents of files and write them (or collect errors and bail)
    replaceses.map{ reps => synthFile(testses, reps, oracle) } match { case fs =>
      val gs = fs.map{
        case Right(f) => Try{ f.write } match {
          case Failure(t) => Left(("Could not write to "+f.file.getCanonicalFile.getPath) +: explainException(t))
          case Success(f) => Right(f)
        }
        case x => x
      }
      gs.collect{ case Left(e) => e }.reduceOption(_ ++ _) match {
        case Some(e) => return Left(s"Could not create code for all test files" +: e)
        case None => Right(gs.collect{ case Right(f) => f })  // Important that this is gs so we know which files were written out!
      }
    }
  }  
  
  /** Compile and run a single test, collecting any output.
    * The exit code should indicate whether a step succeeded or failed.
    */
  def executeTest(tf: TestFile, recompile: Boolean = false): Execution = {
    def compile() = {
      val path = tf.file.getCanonicalFile.getPath
      println("Compiling " + path)
      Execution(scalaEnv.compile(path), CompileTest)
    }
    
    def run() = {
      val result = Execution(scalaEnv.run(tf.qualified), RunTest)
      if (result.output.headOption.exists(_ startsWith "No such file or class")) result.copy(exitcode = 1)
      else result
    }
    
    // First see if we need to compile and bail if it doesn't work
    val comp = if (!tf.written && !recompile) Execution.empty else compile()
    if (comp.failed) return comp
      
    // Then run, but if we didn't compile and the run fails, try compiling and re-running
    run() match {
      case x if x.failed && (comp eq Execution.empty) =>
        val comp = compile()
        if (comp.failed) comp else run() + comp
      case ran => ran + comp
    }
  }
  
  /** Compile a bunch of tests, spawning as many processes as given by `executors`.
    * If there's no progress on any executor for ten minutes, we'll try to just abandon
    * them (and return None instead of Some[Execution] as for something that ran).
    */
  def executeTests(tfs: Vector[TestFile], executors: Int = 1, recompile: Boolean = false): Vector[(TestFile, Option[Execution])] = {
    import concurrent._
    import duration._
    import ExecutionContext.Implicits.global
    
    // Accumulate finished test runs here
    val answers = Vector.newBuilder[(TestFile, Option[Execution])]
    
    // Maintain separate lists of running tasks and pending tasks
    var running = tfs.take(1 max executors).map(tf => tf -> Future(executeTest(tf, recompile)))
    var remaining = tfs.drop(running.length)
    
    while (running.nonEmpty) {
      Try{ Await.ready( Future.firstCompletedOf(running.map(_._2)), Duration("10 min") ) } match {
        case Failure(t) =>
          // Nothing happened for ten minutes--throw everyone away and try a new batch
          // Expect that this is very rare; otherwise you'll send the machine load through the roof.
          answers ++= running.map{ case (tf, _) => tf -> None }
          running = remaining.take(1 max executors).map(tf => tf -> Future(executeTest(tf, recompile)))
          remaining = remaining drop running.length
        case _ =>
          // firstCompletedOf assures us that something's finished
          val (done, undone) = running.partition(_._2.isCompleted)
          val extra = remaining.take(done.length min (1 max executors)).map(tf => tf -> Future(executeTest(tf, recompile)))
          remaining = remaining.drop(extra.length)
          
          // Write the completed tests to stdout in addition to saving them
          done.foreach{ case (tf, fu) => 
            val ex = fu.value.flatMap(_.toOption)
            ex.foreach{ e =>
              val wall = if (e.failed) "!!!!!!!!!!" else "----------"
              val bit = wall.take(1) + " "
              println(wall + "\n" + e.output.map(bit + _ + "\n").mkString + wall)
            }
            answers += tf -> ex
          }
          running = undone ++ extra
      }
    }
    
    answers.result
  }
  
}


object Laws {
  // Hoisting of common constants relies upon comment pairs that can be detected
  val hoistOn = "// Hoistable"
  val hoistOff = "// !Hoistable"
  val HoistName = """\s*val\s+(\w+)\s*=.+""".r
  def makeHoists(variants: Seq[String]): Map[String, String] =
    variants.
      groupBy(x => x match { case HoistName(w) => Some(w); case _ => None }).
      mapValues(v => (v, v.toSet)).
      collect { case (Some(w), (xs, xset)) if xset.size == 1 && xs.length > 1 =>
        w -> xset.head
      }
  def hoistOut(code: Vector[String], hoists: Map[String, String]): Vector[String] = {
    val recoded = Vector.newBuilder[String]
    var hoisting = false
    for (s <- code) s match {
      case x if x endsWith hoistOn => hoisting = true; recoded += x.takeWhile(_ == ' ') + "import Hoists._"
      case x if x endsWith hoistOff => hoisting = false
      case HoistName(w) if hoists contains w =>  // Skip
      case x => recoded += x
    }
    recoded.result
  }
  
  // Macros are a convenient way to delimit separate commands to go in different contexts
  val preMacro = ReplaceMacro(Line.empty, "$PRE", "", "")
  val forMacro = ReplaceMacro(Line.empty, "$FOR", "", "")
  val inMacro = ReplaceMacro(Line.empty, "$IN", "", "")
  val lineMacro = ReplaceMacro(Line.empty, "$LINE", "", "")
  
  /** Convert something into an alphabetic identifier (plus underscore) */
  def simple(s: String) = s.map{ c => if (c.isLetter || c.isWhitespace || c.isDigit) c else '_' }

  type Once[A] = TraversableOnce[A]
  
  /** It's nice to have pipe available. */
  implicit class PipeEverythingForCryingOutLoud[A](val underlying: A) extends AnyVal {
    def |>[B](f: A => B): B = f(underlying)
  }
  
  /** Implicits to be used in single-line tests */
  object Implicits {
    implicit class MyToString[A](val underlying: A) extends AnyVal {
      def myToString = underlying match {
        case t: TraversableOnce[_] => t.mkString(t.getClass.getName+"(",",",")")
        case a: Array[_] => a.mkString(a.getClass.getName+"(",",",")")
        case _ => underlying.toString
      }
    }
    implicit class MoreLogic(val underlying: Boolean) extends AnyVal {
      @inline def implies(b: => Boolean) = !underlying || b
      @inline def impliedBy(b: => Boolean) = !b || underlying
    }
    implicit class MoreComparisons[A](val underlying: Once[A]) {
      @inline def theSameAs[B](cb: Once[B]) = theSame(underlying, cb, false)
      @inline def isPartOf[B](cb: Once[B]) = isPart(underlying, cb, false)
    }
    implicit class ArrayComparisons[A](val underlying: Array[A]) {
      @inline def theSameAs[B](cb: Once[B]) = theSame(underlying, cb, false)
      @inline def isPartOf[B](cb: Once[B]) = isPart(underlying, cb, false)
    }      
  }
  
  /** This implements a generalization of equality */
  def countedSetForall[A](xs: Once[A], ys: Once[A], ordered: Boolean = false)(p: (Int,Int) => Boolean) = {
    if (ordered) {
      val b = new collection.mutable.ArrayBuffer[A]
      xs.foreach(b += _)
      val i = b.result.iterator
      i.corresponds(ys)(_ == _)
    }
    else {
      val hx, hy = new collection.mutable.HashMap[A, Int]
      xs.foreach(a => hx(a) = hx.getOrElse(a,0) + 1)
      ys.foreach(a => hy(a) = hy.getOrElse(a,0) + 1)
      p(hx.size, hy.size) && hx.forall{ case (k,n) => p(n, hy.get(k).getOrElse(0)) }
    }
  }
  
  /** Equality that works on either sets or non-sets */
  def theSame[A](xs: Once[A], ys: Once[A], ordered: Boolean = false) = countedSetForall(xs, ys, ordered)(_ == _)
  
  /** Subset relationship on either sets or non-sets */
  def isPart[A](xs: Once[A], ys: Once[A], ordered: Boolean = false) = countedSetForall(xs, ys, ordered)(_ <= _)
  
  /** Makes sure that two things either succeed and are identical, or both fail */
  def succeedsLike[A](xs: Try[A], ys: Try[A]): Boolean = {
    xs match {
      case Success(x) => ys match {
        case Success(y) => x == y
        case _ => false
      }
      case Failure(_) => ys match {
        case Failure(_) => true
        case Success(_) => false
      }
    }
  }
  
  /** Uses class tags to verify that two collection types are the same type */
  def sameType[A,B[A],C,D[C]](ba: B[A], dc: D[C])(implicit evba: scala.reflect.ClassTag[B[A]], evdc: scala.reflect.ClassTag[D[C]]) = {
    evba == evdc
  }
  
  /** Appears at the beginning of every generated file. */
  val autoComment = "// THIS FILE IS AUTO-GENERATED BY Laws.scala, DO NOT EDIT DIRECTLY"
  
  /** Takes a filename and the desired contents; if the two already match, return false.
    * Otherwise, write the desired contents and return true.
    */
  def freshenFile(file: java.io.File, lines: Vector[String]) = {
    val existing = if (!file.exists) Vector() else scala.io.Source.fromFile(file) |> { x => val ans = x.getLines.toVector; x.close; ans }
    if (lines == existing) false
    else {
      val fos = new java.io.FileOutputStream(file)
      val pw = new java.io.PrintWriter(fos)
      lines.foreach(pw.println)
      pw.close
      true
    }
  }
  
  /** Convert an exception into a bunch of lines that can be passed around as a Vector[String] */
  def explainException(t: Throwable): Vector[String] = Option(t.getMessage).toVector ++ t.getStackTrace.map("  " + _.toString).toVector

  /** Enum type for different type of external executions */
  sealed trait ExecutionType {}
  /** Ran something of an unknown type */
  case object UnknownExecution extends ExecutionType
  /** Compiled a test */
  case object CompileTest extends ExecutionType
  /** Ran a test */
  case object RunTest extends ExecutionType
  
  /** Encapsulates the results of executing some external routine */
  case class Execution(
    exitcode: Int, elapsed: Double, output: Vector[String], command: Seq[String],
    et: ExecutionType, before: Option[Execution] = None
  ) {
    /** Whether any of the executions failed */
    def failed: Boolean = (exitcode != 0) || before.exists(_.failed)
    
    /** The total time taken by all executions */
    def time: Double = elapsed + before.map(_.time).getOrElse(0.0)
    
    /** Link two executions. */
    def +(e: Execution): Execution = e match {
      case x if x eq Execution.empty => this
      case _ => this match {
        case x if x eq Execution.empty => e
        case _ => before match {
          case None => copy(before = Some(e))
          case Some(x) => copy(before = Some(x + e))
        }
      }
    }
  }
  object Execution {
    /** Marker for no actual execution */
    lazy val empty = Execution(0, 0.0, Vector.empty[String], Seq.empty[String], UnknownExecution)
    
    /** Run something (pre-parsed into separate arguments).  Warning, blocks until complete! */
    def apply(et: ExecutionType)(s: String*): Execution = {
      val tstart = System.nanoTime
      val log = Vector.newBuilder[String]
      val logger = sys.process.ProcessLogger(log += _)
      val exitcode = sys.process.Process(s.toSeq).!(logger)
      val elapsed = 1e-9 * (System.nanoTime - tstart)
      Execution(exitcode, elapsed, log.result, s.toSeq, et)
    }
    def apply(ss: Seq[String], et: ExecutionType): Execution = apply(et)(ss: _*)
  }

  /** Write a time in seconds in a form easier for people to understand */
  def humanReadableTime(d: Double): String = {
    // Code is not very DRY, is it?
    val days = (d/86400).floor.toInt
    val hours = ((d - 86400*days)/3600).floor.toInt
    val minutes = ((d - 86400*days - 3600*hours)/60).floor.toInt
    val seconds = (d - 86400*days - 3600*hours - 60*minutes)
    if (days > 0) f"$days days, $hours hours, $minutes minutes, $seconds%.1f seconds"
    else if (hours > 0) f"$hours hours, $minutes minutes, $seconds%.1f seconds"
    else if (minutes > 0) f"$minutes minutes, $seconds%.1f seconds"
    else f"$seconds%.2f seconds"
  }
  
  /** Specifies how to run the Scala compiler and launch the JVM */
  case class ScalaEnv(scala: String, scalaArgs: Seq[String], scalac: String, scalacArgs: Seq[String]) {
    /** Returns the command-line which will compile the files listed in ss */
    def compile(ss: String*): Vector[String] = ((scalac +: scalacArgs) ++ ss.toSeq).toVector
    /** Returns the command-line which will run the files listed in ss */
    def run(ss: String*): Vector[String] = ((scala +: scalaArgs) ++ ss.toSeq).toVector
  }
  
  /** Creates the tests and may run them, depending on command-line options.
    */
  def main(args: Array[String]) {
    // Parse arguments: anything starting with -- is an option, unless
    // it appears after a bare -- in argument list (standard GNU style).
    // Options of the form --blah=foo have a Right("foo") associated with them
    // Options of the form --blah=7 have a Left(7) associated with them
    // Options without = have a Right("").
    val (optable, literal) = args.span(_ != "--")
    val opts = optable.filter(_ startsWith "--").map(_.drop(2)).map{ x =>
      val i = x.indexOf('=')
      if (i < 0) x -> Right("")
      else {
        val y = x.substring(0,i)
        val z = x.substring(i+1)
        Try{ z.toLong } match {
          case Failure(_) => y -> Right(z)
          case Success(n) => y -> Left(n)
        }
      }
    }
    val fnames = optable.filterNot(_ startsWith "--") ++ literal.drop(1)
    
    if (fnames.length != 2) throw new IllegalArgumentException("Need two arguments--replacements file and single-line tests file")
    
    // Remove these flags from collections--an easy way to test if known bugs have been fixed!
    val deflag = opts.collect{ case ("deflag", Right(v)) if v != "" => v }.toSet
    
    // Let the caller specify what scala to use (if actually java, will need to change arguments)
    val scalaCmd = opts.collect{ case ("scala", Right(v)) if v != "" => v }.toList match {
      case cmds @ List(x, y, _*) =>
        throw new IllegalArgumentException("Don't know what to do with more than one version of scala specified: " + cmds.mkString(", "))
      case cmd :: Nil            => cmd
      case Nil                   => "scala"
    }
    
    // And what scalac to use
    val scalacCmd = opts.collect{ case ("scalac", Right(v)) if v != "" => v}.toList match {
      case cmds @ List(x, y, _*) =>
        throw new IllegalArgumentException("Don't know what to do with more than one version of scalac specified: " + cmds.mkString(", "))
      case cmd :: Nil            => cmd
      case Nil                   => "scalac"
    }

    // Let the caller specify what scala arguments to use
    val scalaArgs = opts.collect{ case ("scala-args", Right(v)) if v != "" => v }.toList match {
      case Nil => "-J-Xmx1G" :: Nil
      case cmds => cmds
    }
    
    // And what scalac arguments to use
    val scalacArgs = opts.collect{ case ("scalac-args", Right(v)) if v != "" => v }.toList match {
      case Nil => "-J-Xmx1G" :: Nil
      case cmds => cmds
    }
    
    val scalaEnv = ScalaEnv(scalaCmd, scalaArgs, scalacCmd, scalacArgs)

    // Get the instance that does all the work (save for summarizing)
    val laws = new Laws(false, fnames(0), fnames(1), deflag, scalaEnv)
    
    val tfs: Vector[laws.TestFile] = laws.generateTests match {
      case Left(e) =>
        // If we weren't able to generate tests, bail out
        e.foreach(println)
        sys.exit(1)
        
      case Right(rs) =>
        // Tests were created, so gather some summary statistics and babble about what happened
        var allLines = Set[Int]()
        rs.foreach{ r =>
          allLines |= r.lines
          println(s"Created ${r.lines.size} single-line tests for")
          println(s"  ${r.title}")
          for (u <- r.unvisited if u.nonEmpty) {
            println(s"  ${u.size} methods not tested:")
            // All this is just for prettyprinting method names
            val i = u.toList.sorted.iterator
            var s = "  "
            while (i.hasNext) {
              val t = {
                val x = i.next
                val more = 11 - (x.length % 11)
                x + " "*more
              }
              if (s.length == 2) s += t
              else if (s.length + t.length > 79) {
                println(s)
                s = "  " + t
              }
              else s += t
            }
            if (s.length > 2) println(s)
            // Done prettyprinting
          }
          println
        }
        
        val unusedLines = 
          laws.loadedTestses.right.toOption.
          map(_.flatMap(_.underlying.filter(x => !allLines.contains(x.line.index)))).
          getOrElse(Vector())
        
        println("Generated " + rs.length + " test files.")
        if (unusedLines.nonEmpty) {
          // Report which tests weren't used at all (an unused test is probably an indication of some sort of mistake)
          println(s"${unusedLines.length} test lines were not used for any collection: ")
          unusedLines.toList.sortBy(_.line.index).foreach{ ul =>
            val text = f"${ul.line.index}%-3d ${ul.line.whole}"
            if (text.length < 78) println("  " + text)
            else println("  " + text.take(74) + "...")
          }
        }
        
        // If we want only new tests, filter out only ones whose source changed
        if (opts.exists(_._1 == "changed")) rs.filter(_.written) else rs
    }
    
    opts.find(_._1 == "run").foreach{ case (_, nr) =>
      // Only run the tests if the option --run was given (--run=n says how many processes to spawn simultaneously)
      val n = 1 max nr.left.toOption.getOrElse(1L).toInt
      val tstart = System.nanoTime
      val ran = laws.executeTests(tfs, n, opts.exists(_._1 == "recompile")).sortBy(_._1.qualified)
      val elapsed = 1e-9*(System.nanoTime - tstart)
      
      // When we reach here everything that will run has, so we just need to report on it
      println
      println("========= Summary of Test Run =========")
      println(f"Tested ${ran.length} collections in ${humanReadableTime(elapsed)}")
      
      val succeeded = ran.collect{ case (tf, Some(e)) if !e.failed => (tf,e) }
      println(s"  ${succeeded.length} collections passed")
      
      val ranButFailed = ran.collect{ case (tf, Some(e)) if e.failed && e.et == RunTest => (tf,e) }
      println(s"  ${ranButFailed.length} collections failed")
      ranButFailed.foreach{ case (tf, _) => println("    " + tf.qualified) }
      
      val didntCompile = ran.collect{ case (tf, Some(e)) if e.failed && e.et == CompileTest => (tf,e) }
      println(s"  ${didntCompile.length} collections failed to compile")
      didntCompile.foreach{ case (_, e) => println("    " + e.command.mkString(" ")) }
      
      // These ones are really bad--maybe should limit the number, or provide guidance on how to compile/run by hand?
      val didntEvenFinish = ran.collect{ case (tf, None) => tf }
      if (didntEvenFinish.nonEmpty) {
        println("  ${didntEvenFinish.length} collections got stuck!  Better try these by hand.")
        didntEvenFinish.foreach{ tf => println("    " + tf.qualified) }
      }
      
      // Something weird happened if the previous cases don't add up
      val unaccounted = ran.length - (succeeded.length + ranButFailed.length + didntCompile.length + didntEvenFinish.length)
      if (unaccounted > 0)
        println(s"  Huh?  Couldn't figure out what happened to $unaccounted runs.")
    }
  }
}
