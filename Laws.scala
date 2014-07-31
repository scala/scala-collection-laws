package laws

import scala.language.implicitConversions
import scala.language.higherKinds

import scala.util._
import laws.Parsing._

class Laws(junit: Boolean, replacementsFilename: String, linetestsFilename: String) {
  import Laws._
  
    
  def synthMethod(tests: Tests, replaces: Replacements): Either[Vector[String], (Vector[String], String)] = {
    tests.params.filter(x => !replaces.params.contains(x)) match {
      case missing if missing.nonEmpty => return Left(Vector(
        s"Variables ${missing.toList.sorted.mkString(", ")} missing.",
        s"  Required by test lines ${tests.underlying.map(_.line.index).mkString(", ")}",
        s"  Not provided by collection block starting on line ${replaces.myLine}"
      ))
      case _ =>
    }
    
    val replaced = tests.underlying.map(t => replaces(t.code).right.map(_ -> t.line))
    val broken = replaced.collect{ case Left(x) => x }
    if (broken.nonEmpty) return Left(broken)
    
    // Note: code to right doesn't compile, complains about with! -- val body = iteration +: replaced.collect{ case Right(code, line) => s"assert({$code}, \"Test line ${line.index} with \"+message)" }.flatten
    val iteration = "val message = s\"" + tests.params.toList.sorted.map(x => s"$x = $$$x").mkString("; ") + "\" + blockIdentifierString"
    val body = iteration +: replaced.collect{ case Right((codes, line)) => 
      codes.map{ code =>"assert({" + code + "}, \"Test line " + line.index + " with \"+message)" }
    }.flatten
    Right(body)
    
    val (pres, fors, ins) = {
      val pb, fb, ib = Vector.newBuilder[String]
      tests.params.toList.sorted.map(replaces.params).map(_.value).foreach{ v =>
        preMacro.macroOn(v, "", s => { pb += s; "" })
        forMacro.macroOn(v, "", s => { fb += s; "" })
        inMacro.macroOn(v, "", s => { ib += s; "" })
      }
      (pb.result, fb.result, ib.result)
    }
    val single = List(pres.mkString("\n"), fors.mkString("for (", "; ", ") {"), ins.mkString("\n")).mkString("\r")
    val expanded = replaces(single) match {
      case Left(error) =>
        return Left(Vector(
          "Problem with block starting on line ${replaces.myLine} expanding into test with vars ${tests.params.toList.sorted.mkString(", ")}",
          error
        ))
      case Right(xs) => xs
    }
    
    val whole = Vector.newBuilder[String]
    if (junit) lineMacro.argsOf(replaces.infos.get("junitMethodPrefix").map(_.values.mkString(" ")).getOrElse("")).foreach(whole += _)
    val methodName = "test_" + tests.params.toList.sorted.mkString("_")
    whole += s"def $methodName() {"
    
    expanded.map(_.split("\r",-1)).map{ a =>
      val outer = if (a(0).trim.isEmpty) Vector() else a(0).split("\n").toVector
      val loop = if (a.length < 1 || a(1) == "for () {") None else Some(a(1))
      val myBody = if (a.length < 2 || a(2).isEmpty) body else a(2).split("\n").toVector ++ body
      val inner = if (loop.isDefined) myBody.map("  " + _) else myBody
      outer ++ loop ++ inner ++ loop.map(_ => "}")
    } match {
      case Vector(unique) => whole += "  val blockIdentifierString = \"\""; unique.foreach(whole += "  " + _)
      case more => more.zipWithIndex.foreach{ case (x,i) => 
        whole += "  {"
        whole += "    val blockIdentifierString = \" in group " + (i+1) + "\""
        x.foreach(whole += "    " + _)
        whole += "  }"
      }
    }
    whole += "}"
    Right((whole.result, methodName))
  }
  
  case class TestFile(title: String, pkg: String, code: Vector[String], unvisited: Option[Set[String]], written: Boolean = false) {
    lazy val file = new java.io.File("generated-tests/Test_" + title + ".scala")
    lazy val qualified = (pkg match { case "" => pkg; case _ => pkg + "." }) + "Test_" + title
    def write() = copy(written = freshenFile(file, code))
  }
  def synthFile(testses: Vector[Tests], replaces: Replacements, oracle: Map[String, Set[String]]): Either[Vector[String], TestFile] = {
    val myMethods = replaces("@NAME") match {
      case Right(Vector(name)) => oracle.get(name) match {
        case Some(m) => replaces.infos.get("doNotVerifyMethods").map(m ++ _.values.toSet).getOrElse(m)
        case _ => return Left(Vector(s"Cannot find methods for collection named $name"))
      }
      case _ => return Left(Vector(s"Collection name lookup failed for block at ${replaces.myLine}"))
    }
    val unvisited = new collection.mutable.HashSet[String]
    unvisited ++= myMethods
    replaces.infos.get("doNotVerifyMethods").foreach(unvisited --= _.values)
    
    val flags = replaces.params.get("flags").map(_.value).getOrElse("").split("\\s+").filter(_.nonEmpty).toSet
    val pruned = testses.map(_.filter(x => x.validateFlags(flags) && x.validateMethods(myMethods, unvisited))).filter(_.underlying.nonEmpty) match {
      case Vector() => return Left(Vector(s"No tests match flags set for block starting at line ${replaces.myLine}"))
      case x => x
    }
    val (expanded, named) = {
      val temp = pruned.map(t => synthMethod(t, replaces))
      temp.collect{ case Left(errors) => errors }.reduceOption(_ ++ _) match {
        case Some(errors) => return Left(errors)
        case _ =>
      }
      temp.collect{ case Right(xs) => xs }.unzip
    }
    var title = "unknown"
    val whole = Vector.newBuilder[String]
    whole += autoComment
    lineMacro.argsOf(replaces.infos.get("fileHeader").map(_.values.mkString(" ")).getOrElse("")).foreach(whole += _)
    if (junit) lineMacro.argsOf(replaces.infos.get("junitFileHeader").map(_.values.mkString(" ")).getOrElse("")).foreach(whole += _)
    whole += ""
    whole += (replaces("@NAME") match {
      case Left(error) => return Left(Vector("Could not create name for block starting at line ${replaces.myLine}",error))
      case Right(Vector(x)) => title = simple(x); s"object Test_$title {"
      case Right(xs) => return Left("Could not decide between alternative names for block starting at line ${replaces.myLine}" +: xs)
    })
    expanded.foreach{ e => whole += ""; e.foreach(whole += "  " + _) }
    if (!junit) {
      whole += ""
      whole += "  def main(args: Array[String]) {"
      whole += "    val tests: Vector[() => Unit] = Vector("
      named.map("      " + _ + " _").mkString(",\n").split("\n").foreach(whole += _)
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
    val pkg = lineMacro.argsOf(replaces.infos.get("fileHeader").map(_.values.mkString(" ")).getOrElse("")).
      find(_ startsWith "package ").
      map(_.drop(7).dropWhile(_.isWhitespace)).
      getOrElse("")
    Right(TestFile(title, pkg, whole.result, Some(unvisited.toSet)))
  }
  
  def synthInstances(replaceses: Vector[Replacements]): Either[Vector[String], Vector[String]] = {
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
      ans.collect{ case Left(v) => v }.reduceOption(_ ++ _) match {
        case Some(v) => return Left(v)
        case _ => ans.collect{ case Right(x) => x }
      }
    }
    val assigned = needed.map{ case (name, coll, code) =>
      val v = "inst_" + simple(name)
      (name, v, s"val $v = (classOf[$coll], $code)")
    }
    val whole = Vector.newBuilder[String]
    whole += autoComment
    whole += "package laws"
    whole += "object Instances {"
    assigned.foreach{ case (_, _, code) => whole += "  " + code }
    whole += "  val mapped: Map[String, Set[String]] = Map("
    assigned.foreach{ case (name, v, _) => whole += "    \"" + name + "\" -> laws.MethodFinder(" + v + "._1, " + v + "._2 )," }
    whole += "    \"null\" -> Set()"
    whole += "  )"
    whole += "}"
    Right(whole.result)
  }
  
  def getMethodOracle(replaceses: Vector[Replacements]): Either[Vector[String], Map[String, Set[String]]] = {
    val instanceCode = synthInstances(replaceses) match {
      case Left(v) => return Left("Could not create code for Instances.scala:" +: v)
      case Right(v) => v
    }
    val target = new java.io.File("Instances.scala")
    val changed = Try{ freshenFile(target, instanceCode) } match {
      case Failure(t) => return Left(s"Could not write file ${target.getCanonicalFile.getPath}" +: explainException(t))
      case Success(b) => b
    }
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
    Right(try {
      val obj = Class.forName("laws.Instances$").getDeclaredConstructors.headOption.map{ c => c.setAccessible(true); c.newInstance() }.get
      val meth = Class.forName("laws.Instances$").getDeclaredMethods.find(_.getName == "mapped").get
      meth.invoke(obj).asInstanceOf[Map[String, Set[String]]]
    }
    catch {
      case t: Throwable => return Left("Could not instantiate Instances:" +: explainException(t))
    })
  }
  
  def generateTests(): Either[Vector[String], Vector[TestFile]] = {
    val testses = Tests.read(linetestsFilename) match {
      case Left(e) => return Left(s"Could not read tests filename $linetestsFilename" +: e)
      case Right(ts) => ts
    }
    val replaceses = Replacements.read(replacementsFilename) match {
      case Left(e) => return Left(s"Could not read replacements filename $replacementsFilename" +: e)
      case Right(rs) => rs
    }
    val oracle = getMethodOracle(replaceses) match {
      case Left(e) => return Left(s"Could not acquire available methods:" +: e)
      case Right(ms) => ms
    }
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
  
  def executeTests(files: Vector[TestFile]) {}
  
}
  
object Laws {
  val preMacro = ReplaceMacro(Line.empty, "$PRE", "", "")
  val forMacro = ReplaceMacro(Line.empty, "$FOR", "", "")
  val inMacro = ReplaceMacro(Line.empty, "$IN", "", "")
  val lineMacro = ReplaceMacro(Line.empty, "$LINE", "", "")
  
  def simple(s: String) = s.map{ c => if (c.isLetter || c.isWhitespace || c.isDigit) c else '_' }

  type Once[A] = TraversableOnce[A]
  
  implicit class PipeEverythingForCryingOutLoud[A](val underlying: A) extends AnyVal {
    def |>[B](f: A => B): B = f(underlying)
  }
  
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
  
  class FalseAssertion extends Exception("Assertion failed") {}
  class CountMissed { private[this] var myCount = 0; def inc { myCount += 1 }; def count = myCount }
  
  def insist[A](f: => Boolean, m: => String)(implicit acc: collection.mutable.ArrayBuffer[(String, Throwable)]) {
    try { 
      if (!f) acc += m -> (new FalseAssertion)
    } catch {
      case t: Throwable => acc += m -> t
    }
  }
  
  def reportMissed(implicit acc: collection.mutable.ArrayBuffer[(String, Throwable)], cnt: CountMissed) { if (acc.nonEmpty) {
    cnt.inc
    acc.groupBy{ case (m, t) =>
      m.split(' ').drop(2).take(1).map(_.toInt).apply(0)
    }.toVector.sortBy(_._1).map(_._2).foreach{ mts =>
      val wrong = mts.collect{ case (m, fa: FalseAssertion) => (m, fa) }
      val broke = mts diff wrong
      if (broke.nonEmpty) {
        println("Exceptions thrown:")
        broke.take(20).foreach{ case (m,t) => println("  "+m) }
        broke.drop(20) match { case x => if (x.nonEmpty) println("... and "+x.length+" more") }
        broke.head._2.printStackTrace
      }
      if (wrong.nonEmpty) {
        println("Wrong answers:")
        wrong.take(20).foreach{ case (m,t) => println("  "+m) }
        wrong.drop(20) match { case x => if (x.nonEmpty) println("... and "+x.length+" more") }
        wrong.head._2.printStackTrace
      }
      println
    }
  }}
  
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
  def theSame[A](xs: Once[A], ys: Once[A], ordered: Boolean = false) = countedSetForall(xs, ys, ordered)(_ == _)
  def isPart[A](xs: Once[A], ys: Once[A], ordered: Boolean = false) = countedSetForall(xs, ys, ordered)(_ <= _)
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
  def sameType[A,B[A],C,D[C]](ba: B[A], dc: D[C])(implicit evba: scala.reflect.ClassTag[B[A]], evdc: scala.reflect.ClassTag[D[C]]) = {
    evba == evdc
  }
  
  val autoComment = "// THIS FILE IS AUTO-GENERATED BY Laws.scala, DO NOT EDIT DIRECTLY"
  
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
  
  def explainException(t: Throwable): Vector[String] = Option(t.getMessage).toVector ++ t.getStackTrace.map("  " + _.toString).toVector
  
  def main(args: Array[String]) {
    if (args.length != 2) throw new Exception("Need two arguments--replacements file and single-line tests file")
    val laws = new Laws(false, args(0), args(1))
    laws.generateTests match {
      case Left(e) => e.foreach(println)
      case Right(r) => println("Generated " + r.length + " test files.")
    }
  }
}
