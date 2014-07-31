package laws

import scala.language.implicitConversions
import scala.language.higherKinds

import scala.util._
import laws.Parsing._

/** Generates and runs single-line collections tests that may be viewed as laws that collections should obey. */
class Laws(junit: Boolean, replacementsFilename: String, linetestsFilename: String) {
  import Laws._
  
  /** A method generated for testing. */
  case class TestMethod(name: String, code: Vector[String])
  
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
      codes.map{ code =>"assert({" + code + "}, \"Test line " + line.index + " with \"+message)" }
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
      val outer = if (a(0).trim.isEmpty) Vector() else a(0).split("\n").toVector
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
    Right(TestMethod(methodName, whole.result))
  }

  /** A file that performs tests on a single collection */
  case class TestFile(title: String, pkg: String, code: Vector[String], unvisited: Option[Set[String]], written: Boolean = false) {
    lazy val file = new java.io.File("generated-tests/Test_" + title + ".scala")
    lazy val qualified = (pkg match { case "" => pkg; case _ => pkg + "." }) + "Test_" + title
    def write() = copy(written = freshenFile(file, code))
  }
  
  /** Given tests that use different variables and a set of replacements for a single collection,
    * plus a map that lets us look up which methods are available for that collection, generate
    * a test file (or error messages that tell us what went wrong).
    */
  def synthFile(testses: Vector[Tests], replaces: Replacements, oracle: Map[String, Set[String]]): Either[Vector[String], TestFile] = {
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
    val methods = {
      val temp = pruned.map(t => synthMethod(t, replaces))
      temp.collect{ case Left(errors) => errors }.reduceOption(_ ++ _) match {
        case Some(errors) => return Left(errors)
        case _ =>
      }
      temp.collect{ case Right(xs) => xs }
    }
    
    var title = "unknown"   // Hack to pass back collection name info in a less awkward way
    
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
    
    // This is actually everything important: all the test methods
    methods.foreach{ case TestMethod(_,code) => whole += ""; code.foreach(whole += "  " + _) }
    
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
      whole += "      errors.foreach{ e => println; e.getStackTrace.take(10).foreach(println); }"
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
    Right(TestFile(title, pkg, whole.result, Some(unvisited.toSet)))
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
  def getMethodOracle(replaceses: Vector[Replacements]): Either[Vector[String], Map[String, Set[String]]] = {
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
    if (changed) {
      Try {
        println("Compiling Instances.scala.")
        scala.sys.process.Process(Seq("scalac", "-J-Xmx1G", "Instances.scala"), (new java.io.File(".")).getCanonicalFile).!
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
      case t: Throwable => Left("Could not instantiate Instances:" +: explainException(t))
    }
  }
  
  /** Loads all the files/info needed and generates the tests, including writing the files if their contents have changed. */
  def generateTests(): Either[Vector[String], Vector[TestFile]] = {
    // Load single-line tests (or bail)
    val testses = Tests.read(linetestsFilename) match {
      case Left(e) => return Left(s"Could not read tests filename $linetestsFilename" +: e)
      case Right(ts) => ts
    }
    
    // Load replacements for each collection (or bail)
    val replaceses = Replacements.read(replacementsFilename) match {
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
        case None => Right(fs.collect{ case Right(f) => f })
      }
    }
  }
  
  /** TODO: make this do something. */
  def executeTests(files: Vector[TestFile]) {}
  
}
  
object Laws {
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
  
  /** Create the tests.  Maybe run them, too. */
  def main(args: Array[String]) {
    if (args.length != 2) throw new Exception("Need two arguments--replacements file and single-line tests file")
    val laws = new Laws(false, args(0), args(1))
    laws.generateTests match {
      case Left(e) => e.foreach(println)
      case Right(r) => println("Generated " + r.length + " test files.")
    }
  }
}
