package laws

import scala.language.implicitConversions
import scala.language.higherKinds

import scala.util._
import laws.Parsing._
import laws.{Supply => Sup}
import Sup._

class Laws(junit: Boolean) {
  import Laws._
  
  val containingPackage = "scala.collection"
  
  val universalHeader = autoComment + "\n\n" + {
"""
package """ + containingPackage + """

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.util.Try
import laws.Laws.Implicits._
import laws.Laws.sameType
""" |> { x => if (junit) x else x.split('\n').filter(! _.contains("junit")).mkString("\n") }
  }
  
  val knownSups = Seq(
    Sup("p", "for (i <- ca; p = (_i: @A) => _i < i) {", Wraps, Some(Sup("", "val ca = @CA", Outer))),
    Sup("n", "for (n <- cn) {", Wraps, Some(Sup("","val cn = @CN",Outer))),
    Sup("m", "for (m <- cm) {", Wraps, Some(Sup("","val cm = @CM",Outer))),
    Sup("r", "for (r <- cr) {", Wraps, Some(Sup("","val cr = @CR",Outer))),
    Sup("a", "for (a <- ca) {", Wraps, Some(Sup("","val ca = @CA",Outer))),
    Sup("b", "for (b <- cb) {", Wraps, Some(Sup("","val cb = @CB",Outer))),
    Sup("x", "@LET x = @X", Outer),
    Sup("y", "val ys = @YS", Outer, Some(Sup("", "for (y_i <- ys) { @LET y = y_i()", Wraps))),
    Sup("pf", "for (i <- ca; pf = @PF) {", Wraps, Some(Sup("", "val ca = @CA", Outer))),
    Sup("f", "val f = @F", Outer),
    Sup("g", "val gs = @GS", Outer, Some(Sup("", "for (g <- gs) {", Wraps))),
    Sup("z", "for (z <- ca) {", Wraps, Some(Sup("","val ca = @CA",Outer))),
    Sup("op", "val op = (a1: @A, a2: @A) => a1 @OP a2", Outer),
    Sup("one", "val one = @ONE", Outer),
    Sup("zero", "val zero = @ZERO", Outer)
  )
    
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
        }
        (tame ++ wildMap.filterKeys(k => !tame.contains(k)), substs)
      }
    }
    val elaborated = wildInserted.mapValues(_.map{ case (tame, substs) => (tame /: substs)((t,s) => t.mapValues(s.sub)).toMap })
    elaborated.mapValues(_.map(_.mapValues(s => delimSplit(s).toSet)))
  }
  
  val knownRepls = readReplacementsFile("replacements.tests")
  
  knownRepls.values.toVector.flatten.groupBy(_("CC").head).map{ case (_,v) =>
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
        vs.map{ mm => "    \"" + mm("CC").head + "\" -> (" + mm("instance").head + ", classOf[" + mm("CCM").head + "], mth(" + mm("instance").head + "))," }
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
    val coll = rep("CC").head
    val collname: String = coll.map{ case x if x.isLetter => x; case _ => '_' }
    val flags = rep.getOrElse("flags", Set.empty[String])
    val instance = Instances.all.getOrElse(coll, throw new IllegalArgumentException("Can't find instance "+coll))
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
    collname -> methods
  }).toList
  
  lawsAsWildCode.filterNot(_.covered).foreach{ code =>
    println("Test not used by any collection: "+code.core.mkString("; "))
  }
  
  val doNotCheck = Set("foreach", "map", "flatMap", "filter", "seq", "par", "equals", "toString", "canEqual")
  knownRepls.foreach{ case (_,v) => v.foreach{ rep =>
    val coll = rep("CC").head
    val instance = Instances.all.getOrElse(coll, throw new IllegalArgumentException("Can't find instance "+coll))
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
        pw.println(s"time scalac $source") 
        pw.println
      } }
      run.foreach{ pw => Try {
        val target = s"$containingPackage.$fname" 
        pw.println(s"echo 'Running $target'")
        pw.println(s"time scala $target")
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
        pw.println(s"class $fname {")
        pw.println
        methods.foreach{ m =>
          m.foreach{ line => pw.println("  "+line) }
          pw.println
        }
        pw.println("}")
        if (!junit) {
          pw.println
          pw.println(s"object $fname { def main(args: Array[String]) {")
          pw.println(s"  val tester = new $fname")
          methods.foreach{ m => pw.println("  tester."+m.dropWhile(! _.startsWith("def ")).head.split(' ').tail.head) }
          pw.println("""println("All tests passed for """+fname+"""")""")
          pw.println("}}")
        }
      } finally { pw.close }
    }
    compile.foreach{ pw => Try{ pw.close } }
    run.foreach{ pw => Try { pw.close } }
  }
}
  
object Laws {
  type Once[A] = TraversableOnce[A]
  
  implicit class PipeEverythingForCryingOutLoud[A](val underlying: A) extends AnyVal {
    def |>[B](f: A => B): B = f(underlying)
  }
  
  object Implicits {
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
    
  case class HemiTest(test: String, calls: Set[String], flags: Set[String])
  implicit def testStringToHemiTest(test: String): HemiTest = HemiTest(fixCalls(test), readCalls(test), readFlags(test))
  
  
  def main(args: Array[String]) {
    val laws = new Laws(args contains ("--junit"))
    laws.writeAllTests
  }
}
