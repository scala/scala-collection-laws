package laws

import scala.language.implicitConversions
import scala.language.higherKinds

import scala.util._
import laws.Parsing._

class Laws(junit: Boolean) {
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
    whole += s"def test_$methodName() {"
    
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
  
  def synthFile(testses: Vector[Tests], replaces: Replacements): Either[Vector[String], Vector[String]] = {
    val flags = replaces.params.get("flags").map(_.value).getOrElse("").split("\\s+").filter(_.nonEmpty).toSet
    val pruned = testses.map(_.filter(_.validateFlags(flags))).filter(_.underlying.nonEmpty) match {
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
      whole += "    val tests: Vector[() => ()] = Vector("
      named.map("      " + _ + " _").mkString(",\n").split("\n").foreach(whole += _)
      whole += "    )"
      whole += "    val results = tests.map(f => Try(f()))"
      whole += "    val errors = results.collect{ case Failure(t) => t }"
      whole += "    if (errors.nonEmpty) {"
      whole += "      println(errors.length + \" errors!\")"
      whole += "      errors.foreach{ e => println; e.getStackTrace.take(10).foreach(println); }"
      whole += "      sys.exit(1)"
      whole += "    }"
      whole += "    println(\"All tests passed for " + title + "\")"
      whole += "  }"
    }
    whole += "}"
    Right(whole.result)
  }
  
  /*
  def readReplacementsFile(fname: String) = {
    def splitAtLeast(n: Int, sn: (String, Int)) = {
      val parts = sn._1.split("\\s+")
      if (parts.length < n) throw new IllegalArgumentException(s"Need at least $n tokens on line ${sn._2}: ${sn._1}")
      parts
    }
    def splitArrow(s: String, n: Int): (String, String) = {
      val i = s.indexOf("-->")
      if (i < 0) throw new IllegalArgumentException(s"No substitution (-->) found on line $n: $s")
      s.take(i).trim -> s.drop(i+3).trim
    }
    def splitArrow2(si: (String,Int)): (String, String) = splitArrow(si._1, si._2)
    val src = scala.io.Source.fromFile(fname)
    val lines = try {
      src.getLines().toVector.zipWithIndex.filterNot(_._1.startsWith("//")).map(x => (x._1.trim, x._2))
    } finally { src.close }
    val groups = groupDblLineBreak(lines)(_._1.isEmpty).groupBy(x => splitAtLeast(1, x.head)(0))
    val wildGroups = groups.map{ case (k,v) =>
      val (wild, tame) = v.partition(x => splitAtLeast(2,x.head)(1) == "*")
      if (wild.length > 1)
        throw new IllegalArgumentException(s"Conflicting wildcards for $k on lines ${wild.map(_.head._2).mkString(" ")}")
      k -> (wild.headOption.map(_.tail) -> tame)
    }
    val substReady = wildGroups.mapValues{ case (wild, tames) => wild -> tames.map{ tame =>
      val Array(a,cc) = splitAtLeast(2,tame.head).take(2)
      val (subber, subbee) = tame.tail.partition(_._1.trim.startsWith("$"))
      val substs = subber.map{ case (s,n) =>
        val i = s.indexOf("-->")
        if (i < 0) throw new IllegalArgumentException(s"No replacement pattern found on line $n: $s")
        Substitution.from(s.take(i-1).trim, s.drop(i+3).trim)
      }
      (a, cc, substs, subbee) 
    }}
    val wildInserted = substReady.mapValues{ case (wild, tames) =>
      val wildMap = wild.map(_.map{ splitArrow2 }.toMap).getOrElse(Map.empty[String,String])
      tames.map{ case (a, cc, substs, subbee) =>
        val tame = (subbee.map{ splitArrow2 }.toMap + ("A" -> a) + ("CC" -> cc) + ("CCN" -> cc.takeWhile(_ != '['))) |> { t =>
          if (!(t contains "CCM")) t + ("CCM" -> cc) else t
        } |> { t =>
          if (!(t contains "NAME")) t + ("NAME" -> cc) else t
        }
        (tame ++ wildMap.filterKeys(k => !tame.contains(k)), substs)
      }
    }
    val elaborated = wildInserted.mapValues(_.map{ case (tame, substs) => (tame /: substs)((t,s) => t.mapValues(s.sub)).toMap })
    elaborated.mapValues(_.map(_.mapValues(s => delimSplit(s).toSet)))
  }
  
  val knownRepls = readReplacementsFile("replacements.tests")
  
  knownRepls.values.toVector.flatten.groupBy(_("NAME").head).map{ case (_,v) =>
    if (v.length > 1) s"${v.length} duplicates of ${v.head}" else ""
  }.filter(_.nonEmpty).mkString(" ") match {
    case "" =>
    case s => throw new Exception("Found duplicated collections (not allowed): "+s)
  }
  
  val existingInstanceCode = {
    Try {
      val src = scala.io.Source.fromFile("Instances.scala")
      val contents = Try{ src.getLines().toVector }
      src.close
      contents.toOption.getOrElse(Vector.empty[String])
    }.toOption.getOrElse(Vector.empty[String])
  }
  
  val instanceCode =
    Vector(autoComment,"package laws","import scala.reflect.runtime.{universe => ru}","object Instances {",
      "  def mth[T: ru.TypeTag](t: T) = implicitly[ru.TypeTag[T]].tpe.declarations.filter(d => d.isMethod && d.isPublic && !d.isStatic && !d.isConstructor).map(_.name)",
      "  val all = Map(") ++
      knownRepls.toVector.flatMap{ case (_,vs) =>
        vs.map{ mm => "    \"" + mm("NAME").head + "\" -> (" + mm("instance").head + ", classOf[" + mm("CCM").head + "], mth(" + mm("instance").head + "))," }
      }.sorted ++
      Vector("    \"\" -> null","  )", "}")
    
  if (instanceCode != existingInstanceCode) {
    val pw = new java.io.PrintWriter("Instances.scala")
    try { instanceCode.foreach(pw.println) } finally { pw.close }
    throw new Exception("Instance code not up to date.  Regenerated; please recompile Instances.scala.")
  }
  
  val lawsWithNeeds = {
    val src = scala.io.Source.fromFile("single-line.tests")
    try { 
      src.getLines.map(_.trim).zipWithIndex.filter(_._1.length > 0).filterNot(_._1 startsWith "//").
        map(li => li.copy(_1 = li._1.split("//").head : HemiTest)).toVector
    }
    finally { src.close }
  }
  
  val lawsAsWildCode = lawsWithNeeds.map{ case (ht,i) =>
    val code = Code(Seq(), Seq(), Seq((ht.test,i+1)), ht.flags)
    (code /: ht.calls.toList)((c,x) => 
      knownSups.find(_.name == x).map(_.satisfy(c)).getOrElse{ println("Could not find "+x); c }
    )
  }
  
  val lawsAsCode = knownRepls.mapValues(_.map{ rep =>
    val collid = rep("NAME").head
    val coll = rep("CC").head
    val collname: String = collid.map{ case x if x.isLetter => x; case _ => '_' }
    val flags = rep.getOrElse("flags", Set.empty[String])
    val instance = Instances.all.getOrElse(collid, throw new IllegalArgumentException("Can't find instance "+collid))
    val valid = lawsAsWildCode.filter(_.canRunOn(instance._2, flags)).map(_.cover(coll))
    val methods = valid.groupBy(_.pre).toList.map{ case (pre, codes) =>
      val lines = codes.groupBy(_.wrap).toList.flatMap{ case (wrap, somecodes) =>
        somecodes.head.wrap ++ somecodes.head.msg ++ somecodes.flatMap(c => c.core) ++ somecodes.head.rwrap
      }
      val fixed = fixRepls(pre ++ lines, rep)
      val desc: String = "test_" + fixed.head.take(pre.length).map{
        case x if x.startsWith("val ") => x.drop(3).dropWhile(_.isWhitespace).takeWhile(_.isLetter)
        case x if x.startsWith("def ") => x.drop(3).dropWhile(_.isWhitespace).takeWhile(_.isLetter)
        case x => x.filter(_.isLetter)
      }.mkString("_")
      fixed.indices.map(desc + "_" + _) zip fixed
    }
    /*
    val compactedMethods = {
      type Entry = (String, Seq[String])
      def titleCore(e: Entry) = e._1.split('_').dropRight(1).mkString("_")
      def coll(e: Entry) = e._2.head match { 
        case s if s.startsWith("def x = ") || s.startsWith("val x = ") =>
          Some(s.drop("def x = ".length))
        case _ => None
      }
      def mergeOf(ea: Entry, eb: Entry): Option[(String, List[String], Seq[String], String)] = {
        val title = titleCore(ea)
        if (title != titleCore(eb)) None
        else List(coll(ea), coll(eb)).flatten match {
          case cab @ List(ca, cb) =>
            val tail = ea._2.drop(1)
            if (tail == eb._2.drop(1)) Some((title, cab, tail, ea._2.head.take("def x = ".length)))
            else None
          case _ => None
        }
      }
      def compact(es: Seq[Entry], merged: Seq[Entry] = Seq.empty): Seq[Entry] = {
        if (es.isEmpty) merged
        else if (es.lengthCompare(1) == 0) merged :+ es.head
        else {
          val h = es.head
          val t = es.tail
          val m = t.iterator.map(ti => mergeOf(h,ti)).takeWhile(_.isDefined).toVector.flatten
          if (m.length == 0) compact(t, merged :+ h)
          else {
            val title = m.head._1
            val body = m.head._3
            val dv = m.head._4
            val gens = m.map(_._2.toVector).reduce(_ :+ _.last).mkString("List(() => ",", () => ", ").foreach{ generator => ")
            val combo = (title, (gens +: ("  "+dv+"generator()") +: body.map("  "+_)) :+ "}")
            compact(t.drop(m.length), merged :+ combo)
          }
        }
      }
      methods.map(ms => compact(ms))
    }
    */
    collname -> methods
  }).toList
  
  lawsAsWildCode.filterNot(_.covered).foreach{ code =>
    println("Test not used by any collection: "+code.core.mkString("; "))
  }
  
  knownRepls.foreach{ case (_,v) => v.foreach{ rep =>
    val collid = rep("NAME").head
    val doNotCheck = rep("doNotVerifyMethods").toSet
    val instance = Instances.all.getOrElse(collid, throw new IllegalArgumentException("Can't find instance "+collid))
    val k = instance._2
    if (!Code.filledNeed.contains(k)) {
      println("Could not find any tests covering methods of "+k)
    }
    else {
      val hittable = instance._3.map(nm => Code.dedollar(nm.toString)).filterNot(_ contains "$").toSet
      val missed = hittable -- Code.filledNeed(k) -- doNotCheck
      if (missed.size > 0) {
        println(s"Missed ${missed.size} methods on ${k.getName}")
        missed.toList.sorted.map("  " + _).foreach(println)
      }
    }
  }}
  
  val lawsAsMethods = lawsAsCode.flatMap(_._2).map{ case (fname, methods) => fname -> {
    methods.flatten.map{ case (mname, lines) => 
      Vector(if (junit) "@Test" else "", s"def $mname() {") ++ lines.map("  "+_) :+ "}" 
    }
  }}

  def writeAllTests() {
    val compile = if (junit) None else Try{ new java.io.PrintWriter("compile_tests.sh") }.toOption
    val run = if (junit) None else Try{ new java.io.PrintWriter("run_tests.sh") }.toOption
    val dirname = "generated-tests"
    lawsAsMethods.foreach{ case (fname, methods) =>
      val source = s"$dirname/$fname.scala"
      compile.foreach{ pw => Try{ 
        pw.println(s"echo 'Compiling $source'")
        pw.println(s"time scc -J-Xmx1G $source") 
        pw.println
      } }
      run.foreach{ pw => Try {
        val target = s"$containingPackage.$fname" 
        pw.println(s"echo 'Running $target'")
        pw.println(s"time sca $target")
        pw.println
      } }
      println(s"Writing $fname")
      val testf = new java.io.File(dirname)
      if (!testf.exists) testf.mkdir
      val pw = new java.io.PrintWriter(source)
      try {
        pw.println(universalHeader)
        pw.println
        if (junit) pw.println(s"@RunWith(classOf[JUnit4])")
        else {
          pw.println(s"import laws.Laws.{insist => assert}")
          pw.println(s"import laws.Laws.reportMissed")
          pw.println
        }
        pw.println(s"class $fname {")
        if (!junit) pw.println(s"  implicit val missMe = new laws.Laws.CountMissed")
        pw.println
        methods.foreach{ m =>
          val ma = m.toArray
          var missered = false
          for (i <- ma.indices) {
            val line = ma(i)
            if (!junit && i == ma.length-1) pw.println("    reportMissed")
            pw.println("  "+line)
            if (!junit && !missered && line.startsWith("def test")) {
              pw.println("    implicit val misser = new collection.mutable.ArrayBuffer[(String,Throwable)]")
            }
          }
          pw.println
        }
        pw.println("}")
        if (!junit) {
          pw.println
          pw.println(s"object $fname { def main(args: Array[String]) {")
          pw.println(s"  val tester = new $fname")
          methods.foreach{ m => pw.println("  tester."+m.dropWhile(! _.startsWith("def ")).head.split(' ').tail.head) }
          pw.println("""  if (tester.missMe.count == 0) println("All tests passed for """+fname+"""")""")
          pw.println("""  else println(s"Missed ${tester.missMe.count} method invocations.")""")
          pw.println("}}")
        }
      } finally { pw.close }
    }
    compile.foreach{ pw => Try{ pw.close } }
    run.foreach{ pw => Try { pw.close } }
  }
  */
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
  
  /*
  case class HemiTest(test: String, calls: Set[String], flags: Set[String])
  implicit def testStringToHemiTest(test: String): HemiTest = HemiTest(fixCalls(test), readCalls(test), readFlags(test))
  
  
  def main(args: Array[String]) {
    val laws = new Laws(args contains ("--junit"))
    laws.writeAllTests
  }
  */
}
