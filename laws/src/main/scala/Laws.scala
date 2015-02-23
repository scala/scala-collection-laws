package laws

import scala.language.implicitConversions
import scala.language.higherKinds

import java.io.File

import scala.util._
import scala.collection.mutable.{AnyRefMap => RMap}
import laws.Parsing._

/** Generates and runs single-line collections tests that may be viewed as laws that collections should obey. */
class Laws(replacementsRaw: Vector[String], linetestsRaw: Vector[String], globalFlags: Parsing.Flags) {
  import Laws._
  
  def deflag = globalFlags.neg
  
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
    Right(TestMethod(methodName, whole.result, tests.underlying.map(_.line.index).toSet))
  }

  /** A file that performs tests on a single collection */
  case class TestFile(
    title: String, pkg: String, code: Vector[String],
    unvisited: Option[Set[String]], lines: Set[Int],
    dir: File, written: Boolean = false
  ) {
    lazy val file = new File(dir, "Test_" + title + ".scala")
    lazy val qualified = (pkg match { case "" => pkg; case _ => pkg + "." }) + "Test_" + title
    def write() = copy(written = freshenFile(file, code))
  }
  
  /** Given tests that use different variables and a set of replacements for a single collection,
    * plus a map that lets us look up which methods are available for that collection, generate
    * a test file (or error messages that tell us what went wrong).
    */
  def synthFile(testses: Vector[Tests], replaces: Replacements, oracle: Map[String, Set[String]], dir: File): Either[Vector[String], TestFile] = {
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
    val flags = replaces.flags.pos
    
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
    whole += "import laws.Laws.{RunnableLawsTest, RunnableLawsResult}"
    whole += ""
    whole += (replaces("@NAME") match {
      // Order is important here!  Only the single-entry vector case is good.
      case Right(Vector(x)) => title = simple(x); s"object Test_$title extends RunnableLawsTest {"
      case Right(xs) => return Left("Could not decide between alternative names for block starting at line ${replaces.myLine}" +: xs)
      case Left(error) => return Left(Vector("Could not create name for block starting at line ${replaces.myLine}",error))
    })
    
    // Include any object header lines
    lineMacro.argsOf(replaces.infos.get("objectHeader").map(_.values.mkString(" ")).getOrElse("")).foreach(whole += "  " + _)
    
    // Include all the test methods
    methods.foreach{ case TestMethod(_, code, _) => whole += ""; code.foreach(whole += "  " + _) }
    
    // Create a run method that runs everything
    whole += ""
    whole += "  def run(): RunnableLawsResult = {"
    whole += "    val tests: Vector[(() => Unit, Set[Int])] = Vector("
    methods.map(m => "      (" + m.name + " _, Set[Int](" + m.lines.mkString(", ") + "))").mkString(",\n").split("\n").foreach(whole += _)
    whole += "    )"
    whole += "    val results = tests.map{ case (f, ns) => scala.util.Try{ f(); ns } }"
    whole += "    val errors = results.collect{ case scala.util.Failure(t) => t }"
    whole += "    val successes = results.collect{ case scala.util.Success(s) => s }"
    whole += "    RunnableLawsResult(errors, successes.fold(Set.empty[Int])(_ | _), \"" + title + "\")"
    whole += "  }"
    
    // Create a main method that can be used for a standalone run
    whole += "  "
    whole += "  def main(args: Array[String]) {"
    whole += "    val ran = run()"
    whole += "    if (ran.errors.nonEmpty) {"
    whole += "      println(ran.errors.length + \" errors for \" + ran.title + \"!\")"
    whole += "      ran.errors.foreach{ e => println; laws.Laws.explainException(e).take(10).foreach(println); }"
    whole += "      sys.exit(1)"
    whole += "    }"
    whole += "    println(\"All tests passed for \" + ran.title)"
    whole += "  }"
    whole += "}"
    
    // Find the package name from the fileHeader info
    val pkg = lineMacro.argsOf(replaces.infos.get("fileHeader").map(_.values.mkString(" ")).getOrElse("")).
      find(_ startsWith "package ").
      map(_.drop(7).dropWhile(_.isWhitespace)).
      getOrElse("")
      
    // We have successfully created a test file.
    Right(TestFile(
      title, 
      pkg,
      whole.result,
      Some(unvisited.toSet),
      methods.map(_.lines).reduceOption(_ ++ _).getOrElse(Set()),
      dir
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
  
  /** Generate the Instances.scala file if it isn't visible, otherwise via reflection load it to get method info */
  def getOrCreateMethodOracle(replaceses: Vector[Replacements], generate: Option[File]): Either[Vector[String], Map[String, Set[String]]] = {
    // Load map with reflection or catch the mistake and say what went wrong
    val wrong: Throwable = {
      try {
        val obj = Class.forName("laws.Instances$").getDeclaredConstructors.headOption.map{ c => c.setAccessible(true); c.newInstance() }.get
        val meth = Class.forName("laws.Instances$").getDeclaredMethods.find(_.getName == "mapped").get
        val ans = Right(meth.invoke(obj).asInstanceOf[Map[String, Set[String]]])
        // If laws.Instances isn't around, we'll have bailed out with an exception by this point
        // S
        return (
          if (generate.isEmpty) ans
          else Left(Vector("Stale laws.Instances found visible to generator for Scala.instances??"))
        )
      }
      catch { case t: Throwable => t }
    }
    
    // If we got here, we do not have laws.Instances available, and we weren't supposed to, so we proceed.
    // Make the code or bail if something went wrong
    val instanceCode = synthInstances(replaceses) match {
      case Left(v) => return Left("Could not create code for Instances.scala:" +: v)
      case Right(v) => v
    }
    
    // Create the file (or bail if something went wrong)
    
    val target = generate match {
      case Some(x) if x.getName.endsWith("Instances.scala") => x
      case Some(x) if x.isDirectory => new File(x, "Instances.scala")
      case Some(x) => return Left(Vector("Target for Instances.scala incorrect: " + x.getPath))
      case None => return Left(Vector("Should never reach here?!"))
    }
    
    Try{ freshenFile(target, instanceCode) } match {
      case Failure(t) => Left(s"Could not write file ${target.getCanonicalFile.getPath}" +: explainException(t))
      case Success(b) => Right(Map.empty)
    }
  }
  
  lazy val loadedTestses = Tests.read(linetestsRaw)
  lazy val loadedReplacementses = Replacements.read(replacementsRaw).right.map{ rs =>
    rs.map{ r => r.copy(flags = globalFlags :++ r.flags) }.filter{ r => !r.flags("BROKEN") }
  }
  
  /** Loads all the files/info needed and generates the tests, including writing the files if their contents have changed. */
  def generateTests(target: Either[File, File]): Either[Vector[String], Vector[TestFile]] = {
    // Load single-line tests (or bail)
    val testses = loadedTestses match {
      case Left(e) => return Left(s"Could not read $singleLineName" +: e)
      case Right(ts) => ts
    }
    
    // Load replacements for each collection (or bail)
    val replaceses = loadedReplacementses match {
      case Left(e) => return Left(s"Could not read $replacementsName" +: e)
      case Right(rs) => rs
    }
    
    // Load or generate Instances (or bail)
    val oracle = getOrCreateMethodOracle(replaceses, target.left.toOption) match {
      case Left(e) => return Left(s"Could not acquire available methods:" +: e)
      case Right(ms) => ms
    }
    
    target match {
      case Left(_) => Right(Vector.empty[TestFile])
      case Right(dir) if (!dir.isDirectory) => Left(Vector(dir.getPath + " is not a directory--no place to put generated tests!"))
      case Right(dir) =>
        // Create contents of files and write them (or collect errors and bail)
        replaceses.map{ reps => synthFile(testses, reps, oracle, dir) } match { case fs =>
          val gs = fs.map{
            case Right(f) => Try{ f.write } match {
              case Failure(t) => Left(("Could not write to "+f.file.getCanonicalFile.getPath) +: explainException(t))
              case Success(f) => Right(f)
            }
            case x => x
          }
          val tests = gs.collect{ case Left(e) => e }.reduceOption(_ ++ _) match {
            case Some(e) => return Left(s"Could not create code for all test files" +: e)
            case None => gs.collect{ case Right(f) => f }  // Important that this is gs so we know which files were written out!
          }
          val all = Vector.newBuilder[String]
          all += "package tests.generated.collection"
          all += "import laws.Laws.{RunnableLawsTest, RunnableLawsResult}"
          all += "object Test_All {"
          all += testses.flatMap(_.tests.map(_.line.index)).foldLeft(Set.empty[Int])(_ + _).mkString("  val lines = Set(", " ,", ")")
          all += "  val tests = Vector[RunnableLawsTest]("
          tests.map("    Test_" + _.title).mkString(",\n").split('\n').foreach(all += _)
          all += "  )"
          all += ""
          all += "  def main(args: Array[String]) {"
          all += "    val results = tests.map(_.run())"
          all += "    val exitcode = laws.Laws.reportResults(results, lines)"
          all += "    if (exitcode != 0) sys.exit(exitcode)"
          all += "  }"
          all += "}"
          freshenFile(new File(dir, "Test_All.scala"), all.result)
          Right(tests)
        }
    }
  }  
}


object Laws {
  // Constants
  val singleLineName = "single-line.tests"
  val replacementsName = "replacements.tests"
  val flagMapName = "flag-versions.map"

  // Macros are a convenient way to delimit separate commands to go in different contexts
  val preMacro = ReplaceMacro(Line.empty, "$PRE", "", "")
  val forMacro = ReplaceMacro(Line.empty, "$FOR", "", "")
  val inMacro = ReplaceMacro(Line.empty, "$IN", "", "")
  val lineMacro = ReplaceMacro(Line.empty, "$LINE", "", "")
  
  /** Convert something into an alphabetic identifier (plus underscore) */
  def simple(s: String) = s.map{ c => if (c.isLetter || c.isWhitespace || c.isDigit) c else '_' }

  type Once[A] = TraversableOnce[A]
  
  case class RunnableLawsResult(errors: Vector[Throwable], lines: Set[Int], title: String);
  trait RunnableLawsTest { def run(): RunnableLawsResult }
  
  private val bigWall = "#####"
  private val smallWall = "=/|\\="
  private def boxPrint(in: Vector[String], wall: String, wallSize: Int = 10) = {
    require(wall.length == 5)
    val boxed = Vector.newBuilder[String]
    val qq = wall.substring(2,3) + " "
    boxed += wall.substring(1,2) + (if (wallSize > 1) wall.substring(0,1)*(wallSize-1) else "")
    in.foreach{ s => boxed += qq + s }
    boxed += wall.substring(3,4) + (if (wallSize > 1) wall.substring(4,5)*(wallSize-1) else "")
    boxed.result
  }
  
  def reportResults(results: Vector[RunnableLawsResult], inputLines: Set[Int]): Int = {
    val (errors, successes) = results.partition(_.errors.nonEmpty)
    val asserted = errors.map(e => e.copy(errors = e.errors.collect{ case a: AssertionError => a })).filter(_.errors.nonEmpty)
    val crashes = errors.map(e => e.copy(errors = e.errors.filter{ case a: AssertionError => false; case _ => true })).filter(_.errors.nonEmpty)
    
    val lineSuccess = results.flatMap(_.lines.toList).groupBy(identity).map{ case (k,v) => v.size }
    val successPerCollection = lineSuccess.sum / lineSuccess.size
    val successSummary =
      s"${successes.length} collections run against suite of ${lineSuccess.size} tests (${lineSuccess.sum} total)"
    
    val missingLines = results.map(_.lines).foldLeft(inputLines)(_ diff _).toList.sorted
    val missingLinesSummary = s"Test lines ${missingLines.mkString(", ")} were not used."
    
    val failureSummary = s"${errors.length} collections failed (${crashes.length} with crashes, ${asserted.length} with failed assertions)"
    val assertedWithLines = asserted.map{ a =>
      a.errors.map(_.getMessage.split("\\s+").drop(2).headOption.flatMap{ x => Try{ x.toInt }.toOption }) -> a
    }
    val longCrashSummary = {
      val msg = Vector.newBuilder[String]
      msg += s"${crashes.length} collections crashed during tests with an unanticipated exception:"
      crashes.foreach{ c =>
        msg += s"  ${c.title}"
      }
      msg += "Details follow."
      var ndetails = 0
      val details = crashes.
        map{ c => 
          val errorList = c.errors.map(explainException).reduceOption((l,r) => (l :+ "") ++ r)
          ndetails += 1
          boxPrint(errorList.getOrElse(Vector.empty), smallWall)
        }.
        reduceOption((l,r) => (l :+ "") ++ r).
        getOrElse(Vector.empty)
      msg ++= (if (ndetails > 1) boxPrint(details, bigWall, 20) else details)
      msg += ""
      msg += ""
      msg.result
    }
    val longAssertionSummary = {
      val msg = Vector.newBuilder[String]
      msg += s"${asserted.length} collections failed assertions:"
      assertedWithLines.foreach{ case (ns, a) =>
        msg += s"  ${a.title} on lines ${ns.flatten.mkString(" ")}"
      }
      msg += "Details follow."
      var ndetails = 0
      val details = asserted.
        map{ a =>
          val errorList = a.errors.
            map{ t => explainException(t).take(3) :+ "..." }.
            reduceOption((l,r) => (l :+ "") ++ r)
          ndetails += 1
          boxPrint(errorList.getOrElse(Vector.empty), smallWall)
        }.
        reduceOption((l,r) => (l :+ "") ++ r).
        getOrElse(Vector.empty)
      msg ++= (if (ndetails > 1) boxPrint(details, bigWall, 20) else details)
      msg += ""
      msg += ""
      msg.result
    }
    
    if (crashes.nonEmpty) longCrashSummary.foreach(println)
    if (asserted.nonEmpty) longAssertionSummary.foreach(println)
    if (errors.nonEmpty) println(failureSummary)
    if (missingLines.nonEmpty) println(missingLinesSummary)
    println(successSummary)
    
    if (errors.nonEmpty) 1 else 0  // Recommended exit code
  }
  
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
  
  /** Catches _EVERYTHING_ */
  def tryE[A](f: => A): Either[Throwable, A] = try { Right(f) } catch { case t: Throwable => Left(t) }
  
  /** Catches _EVERYTHING_ and you don't know what it was */
  def tryO[A](f: => A): Option[A] = try { Some(f) } catch { case t: Throwable => None }
  
  /** Uses class tags to verify that two collection types are the same type */
  def sameType[A,B[A],C,D[C]](ba: B[A], dc: D[C])(implicit evba: scala.reflect.ClassTag[B[A]], evdc: scala.reflect.ClassTag[D[C]]) = {
    evba == evdc
  }
  
  /** Appears at the beginning of every generated file. */
  val autoComment = "// THIS FILE IS AUTO-GENERATED BY Laws.scala, DO NOT EDIT DIRECTLY"
  
  /** Takes a filename and the desired contents; if the two already match, return false.
    * Otherwise, write the desired contents and return true.
    */
  def freshenFile(file: File, lines: Vector[String]) = {
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
  def explainException(t: Throwable): Vector[String] = Vector(t.getClass.getName) ++ Option(t.getMessage).toVector ++ { t match {
    case so: StackOverflowError => 
      // Stack overflows are really long, so don't print the boring repetitive part in the middle
      val trace = so.getStackTrace.map("  " + _.toString)
      val seen = RMap[String, Int]()
      var front, i, k = 0
      while (k < 3 && front < trace.length) {
        val line = trace(front)
        seen += line -> { k = seen.getOrElse(line,0) + 1; k }
        front += 1
      }
      var back = trace.length - 1
      k = 0
      seen.clear
      while (k < 3 && back >= 0) {
        val line = trace(back)
        seen += line -> { k = seen.getOrElse(line,0) + 1; k }
        back -= 1
      }
      val redacted =
        if (front >= back - 1) trace
        else ((trace.slice(0,front+1) :+ s"  ... lines ${front+1} to ${back-1} ...") ++ trace.slice(back,trace.length-1))
      redacted.toVector
    case _ => t.getStackTrace.map("  " + _.toString).toVector
  }}

  
  /** Creates the tests or the Instances.scala file, depending on command-line options.
    */
  def main(args: Array[String]) {
    /** Gets lines of text from a file on the classpath */
    def getResourceImpl(r: String, die: Boolean) = Try{
      val in = io.Source.fromInputStream(this.getClass.getResourceAsStream(r))
      try { in.getLines.toVector }
      finally { in.close() }
    } match {
      case Success(lines) => Some(lines)
      case Failure(x) => 
        if (die) {
          x.printStackTrace
          sys.exit(1)
        } 
        else None
    }
    
    def getResource(r: String) = getResourceImpl(r, true).orNull
    def getResourceOptionally(r: String) = getResourceImpl(r, false)
    
    val testLines = getResource("/" + singleLineName)
    val replaceLines = getResource("/" + replacementsName)
    val flagLines = getResourceOptionally("/" + flagMapName).getOrElse(Vector.empty)
    
    // Command-line option handling, long-form GNU style
    val (optable, literal) = args.span(_ != "--")
    val (optish, notoptish) = optable.partition(_ startsWith "--")
    val opts = optish.map(_.drop(2)).map{ x =>
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
    
    // There should be exactly one target (the directory in which to write the generated file(s))
    val target = (notoptish ++ literal.dropWhile(_ == "--")).toList match {
      case f :: Nil => new File(f)
      case Nil =>
        throw new IllegalArgumentException("No output target provided.")
      case fs =>
        throw new IllegalArgumentException("Only one output should be provided.  Found ${fs.length}:\n" + fs.map("  " + _).mkString("\n"))
    }
    
    /** Flags passed in on the command line (highest priority). */
    val commandLineFlags =
      List("enflag", "deflag").map{ s => opts.collect{ case (x, Right(flag)) if x == s => flag }.toSet } match {
        case List(pos, neg) => Parsing.Flags(pos, neg)
      }
    
    /** Flags specified in the flag-versions.map file (2nd highest priority). */
    val versionedFlags =
      opts.collectFirst{ case ("versionID", Right(id)) => id }.fold(Parsing.Flags.empty){ ver =>
        val flagsParsed = Lines.anykeyParsed(flagLines).ap(_.filter(_.sep == "-->")).lines
        val duplicates = flagsParsed.groupBy(_.left).filter(_._2.length > 1)
        if (duplicates.nonEmpty)
          throw new IllegalArgumentException(s"Problem in ${flagMapName}: duplicates found for ${duplicates.keys.toList.sorted.mkString(" ")}")
        
        val relevantMap = flagsParsed.filter(line => ver startsWith line.left).toList.sortBy(- _.left.length).headOption
        relevantMap.
          map(line => Parsing.Flags.parseFromLine(line, true) match {
            case Right(flags) => flags
            case Left(error) => throw new IllegalArgumentException(s"Could not read flags on line ${line.index} of ${flagMapName}: $error")
          }).
          getOrElse(Parsing.Flags.empty)
      }
    
    /** Pack in Left or Right depending on whether we are to generate Instances file. */
    def whichever(f: File) = 
      if (opts exists (_._1 equalsIgnoreCase "Instances")) Left(f)
      else Right(f)
      
    /** Generate laws */
    val laws = new Laws(replaceLines, testLines, commandLineFlags :++ versionedFlags)
    laws.generateTests(whichever(target)) match {
      case Left(x) => x.foreach(println); sys.exit(1)
      case Right(tests) if tests.size > 0 => 
        println(s"Created tests for ${tests.size} conditions.  Method coverage:")
        tests.sortBy(_.title).foreach{ test =>
          val missing = test.unvisited match {
            case Some(x) => if (x.size==0) "complete" else s"missed ${x.size}:"
            case None => ""
          }
          println(s"${test.title}"/* (${if (test.written) "NEW" else "old"})*/+s"; $missing")  // Something is wrong with test.written?
          var indent = 0
          val spots = List(4, 19, 34, 49, 64)
          test.unvisited.filter(_.nonEmpty).foreach{uns =>
            uns.toList.sorted.foreach{ un =>
              val extra = " "*spots.find(_ > indent).map(_ - indent).getOrElse(80)
              if (indent + extra.length + un.length + 1 >= 80) {
                print(s"\n    $un")
                indent = 4 + un.length
              }
              else {
                print(s"$extra$un")
                indent += extra.length + un.length
              }
            }
            println
          }
        }
      case _ =>
    }
    
  }
}
