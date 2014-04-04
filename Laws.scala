package laws

import scala.util._

class Laws(junit: Boolean) {
  import Laws._
  
  val universalHeader = autoComment + "\n\n" + {
"""
package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.util.Try
import laws.Laws.Implicits._
""" |> { x => if (junit) x else x.split('\n').filter(! _.contains("junit")).mkString("\n") }
  }
  
  val knownCalls = Seq(
    Call("p", "for (i <- ca; p = (_i: @A) => _i < i) {", Wraps, Some(Call("", "val ca = @CA", Outer))),
    Call("n", "for (n <- cn) {", Wraps, Some(Call("","val cn = @CN",Outer))),
    Call("m", "for (m <- cm) {", Wraps, Some(Call("","val cm = @CM",Outer))),
    Call("a", "for (a <- ca) {", Wraps, Some(Call("","val ca = @CA",Outer))),
    Call("b", "for (b <- cb) {", Wraps, Some(Call("","val cb = @CB",Outer))),
    Call("x", "@LET x = @X", Outer),
    Call("y", "val ys = @YS", Outer, Some(Call("", "for (y_i <- ys) { @LET y = y_i()", Wraps))),
    Call("pf", "for (i <- ca; pf = @PF) {", Wraps, Some(Call("", "val ca = @CA", Outer))),
    Call("f", "val f = @F", Outer),
    Call("z", "for (z <- ca) {", Wraps, Some(Call("","val ca = @CA",Outer))),
    Call("op", "val op = (a1: @A, a2: @A) => a1 @OP a2", Outer),
    Call("one", "val one = @ONE", Outer),
    Call("zero", "val zero = @ZERO", Outer)
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
    val groups = groupDblLineBreak(lines)(_._1).groupBy(x => splitAtLeast(1, x.head)(0))
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
        DollarSubst.from(s.take(i-1).trim, s.drop(i+3).trim)
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
    elaborated.mapValues(_.map(_.mapValues(s => smartSplit(s).toSet)))
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
    Vector(autoComment,"package laws","object Instances { val all = Map(") ++
    knownRepls.toVector.flatMap{ case (_,vs) =>
      vs.map{ mm => "\"" + mm("CC").head + "\" -> (" + mm("instance").head + ", classOf[" + mm("CCM").head + "])," }
    }.sorted ++
    Vector("  \"\" -> null)", "}")
    
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
    val code = Code(Seq(), Seq(), Seq("assert({" + ht.test + "}, message(" + (i+1) + "))"), Seq(), ht.flags)
    (code /: ht.calls.toList)((c,x) => 
      knownCalls.find(_.name == x).map(_.satisfy(c)).getOrElse{ println("Could not find "+x); c }
    )
  }
  
  val lawsAsCode = knownRepls.mapValues(_.map{ rep =>
    val coll = rep("CC").head
    val collname: String = coll.map{ case x if x.isLetter => x; case _ => '_' }
    val flags = rep.getOrElse("flags", Set.empty[String])
    val instance = Instances.all.getOrElse(coll, throw new IllegalArgumentException("Can't find instance "+coll))
    val valid = lawsAsWildCode.filter(_.canRunOn(instance, flags)).map(_.cover(coll))
    val methods = valid.groupBy(_.pre).toList.map{ case (pre, codes) =>
      val lines = codes.groupBy(_.lwrap).toList.flatMap{ case (lwrap, somecodes) =>
        somecodes.head.lwrap ++ somecodes.head.msg ++ somecodes.flatMap(c => c.core) ++ somecodes.head.rwrap
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

  val lawsAsMethods = lawsAsCode.flatMap(_._2).map{ case (fname, methods) => fname -> {
    methods.flatten.map{ case (mname, lines) => 
      Vector(if (junit) "@Test" else "", s"def $mname() {") ++ lines.map("  "+_) :+ "}" 
    }
  }}

  def writeAllTests() {
    lawsAsMethods.foreach{ case (fname, methods) =>
      println(s"Writing $fname")
      val dirname = "generated-tests"
      val testf = new java.io.File(dirname)
      if (!testf.exists) testf.mkdir
      val pw = new java.io.PrintWriter(s"$dirname/$fname.scala")
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
          pw.println("}}")
        }
      } finally { pw.close }
    }
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
  
  val autoComment = "// THIS FILE IS AUTO-GENERATED BY Laws.scala, DO NOT EDIT DIRECTLY"
  
  val knownDollars = 
    Seq("plus +", "minus -", "times *", "div /", "bslash \\", "colon :", "eq =", "amp &", "tilde ~", "bar |").
    map(_.split(' ') match { case Array(name, sym) => name -> sym })
  def dedollar(s: String, from: Int = 0, prefix: String = ""): String = {
    val i = s.indexOf('$',from)
    if (i < 0 || i+1 >= s.length) prefix + s.substring(from)
    else knownDollars.find{ case (name, _) => s.startsWith(name,i+1) } match {
      case Some((name, sym)) => dedollar(s, i+1+name.length, prefix + s.substring(from,i) + sym)
      case None => dedollar(s, i+1, prefix + s.substring(from, i+1))
    }
  }

  sealed trait Pos
  case object Inner extends Pos
  case object Outer extends Pos
  case object Wraps extends Pos
  
  case class Code(pre: Seq[String], lwrap: Seq[String], in: Seq[String], rwrap: Seq[String], flags: Set[String]) {
    private[this] var covernames = List.empty[String]
    def cover(name: String): this.type = { covernames = name :: covernames; this }
    def covered = covernames.nonEmpty
    def coverage = covernames.reverse.toVector
    def msg = Seq(
      ("  "*rwrap.length) +
      "val message = (lnum: Int) => \"Law line %d;\".format(lnum)" + 
      lwrap.map(_.trim).filter(_.startsWith("for (")).map(_.drop(5).
        takeWhile(_.isLetter)).filter(_.length > 0).map(x => """+" %s="+%s.toString""".format(x,x)).
        mkString
    )
    def core = in.map("  "*rwrap.length + _)
    def join = pre ++ lwrap ++ msg ++ core ++ rwrap
    def canRunOn[C,_](cc: (C, Class[_]), cflags: Set[String]): Boolean = {
      val available = cc._2.getMethods.
        filterNot(x => java.lang.reflect.Modifier.isStatic(x.getModifiers)).
        map(x => dedollar(x.getName)).toSet
      val needs = join.flatMap(readNeeds).toSet
      val (nflags, yflags) = cflags.partition(_.startsWith("!"))
      (needs subsetOf available) && yflags.subsetOf(flags) && (nflags.map(_.drop(1))&flags).isEmpty
    }
  }
  
  case class Call(name: String, code: String, position: Pos, also: Option[Call] = None) {
    def satisfy(lines: Code): Code = {
      val newlines = position match {
        case Outer => lines.copy(pre = code +: lines.pre)
        case Inner => lines.copy(in = code +: lines.in)
        case Wraps => lines.copy(lwrap = code +: lines.lwrap.map("  "+_), rwrap = lines.rwrap.map("  "+_) :+ "}")
      }
      also.map(_.satisfy(newlines)).getOrElse(newlines)
    }
  }
  
  def groupDblLineBreak[A](ls: Seq[A], soFar: List[Seq[A]] = Nil)(f: A => String): List[Seq[A]] = {
    val blank = (a: A) => f(a).isEmpty
    if (ls.isEmpty) {
      soFar.map(_.dropWhile(blank).reverse.dropWhile(blank).reverse).filter(_.nonEmpty)
    }
    else {
      val i = ls.sliding(2).takeWhile(!_.forall(a => f(a).isEmpty)).size
      val (good, bad) = ls.splitAt(i+1)
      groupDblLineBreak(bad, good :: soFar)(f)
    }
  }

  case class DollarSubst(key: String, lhs: String, rhs: String) {
    def sub(s: String, prefix: String): String = if (s.isEmpty) prefix else {
      val i = s.indexOfSlice(key)
      if (i < 0) prefix + s
      else {
        DollarSubst.matchingIndexOf(s, i+key.length) match {
          case None => throw new IllegalArgumentException(s"Could not match $key in $s")
          case Some((il, ir)) =>
            if (ir < 0) throw new IllegalArgumentException(s"Could not find right limit of parameter for $key in $s")
            val (i0,i1) = if (DollarSubst.paired contains s(il)) (il+1,ir) else (il,ir+1)
            sub(s.drop(ir+1), prefix + s.substring(0,i) + lhs + s.slice(i0,i1) + rhs)
        }
      }
    }
    def sub(s: String): String = sub(s,"")
  }
  object DollarSubst {
    def from(key: String, subst: String) = {
      val i = subst.indexOfSlice(" $ ")
      if (i < 0) throw new IllegalArgumentException("Could not find \" $ \" in " + subst)
      val j = subst.indexOfSlice(" $ ",i+3)
      if (j >= 0) throw new IllegalArgumentException("Found more than one \" $ \" in " + subst)
      new DollarSubst(key, subst.substring(0,i), subst.substring(i+3))
    }
    val paired = "()[]{}<>".grouped(2).map(x => x(0) -> x(1)).toMap
    def matchingIndexOf(s: String, i: Int = 0): Option[(Int,Int)] = {
      if (i >= s.length) None
      else paired.get(s(i)) match {
        case None => 
          if (s(i).isWhitespace) matchingIndexOf(s, i+1)
          else Some((i, s.indexWhere(_.isWhitespace, i+1)-1))
        case Some(r) =>
          val l = s(i)
          var depth = 1
          var j = i+1
          while (j < s.length && depth > 0) {
            val c = s(j)
            if (c==r) depth -= 1
            else if (c==l) depth += 1
            j += 1
          }
          if (depth==0) Some((i,j-1)) else None
      }
    }
  }
  
  def smartSplit(s: String, from: Int = 0, found: collection.mutable.Builder[String,Vector[String]] = Vector.newBuilder[String]): Seq[String] = {
    var i = from
    while (i < s.length && s(i).isWhitespace) i += 1
    if (i >= s.length) found.result
    else {
      var j = i
      while (j < s.length && !s(j).isWhitespace) {
        if (DollarSubst.paired.contains(s(j)) || s(j) == '"' || s(j) == '\'') {
          var k = j+1
          val cl = s(j)
          val cr = DollarSubst.paired.get(s(j)).getOrElse(cl)
          var depth = 1
          while (k < s.length && depth > 0) {
            val c = s(k)
            if (c==cr) depth -= 1
            else if (c==cl) depth += 1
            k += 1
          }
          j = k
        }
        else j += 1
      }
      found += s.substring(i,j)
      smartSplit(s, j, found)
    }
  }

  val NeedReg = "`[^`]+`".r
  def readNeeds(s: String) = NeedReg.findAllIn(s).map(x => x.slice(1,x.length-1)).toSet
  
  def readCalls(s: String) = {
    val i = s.indexOf("...")
    if (i >= 0) s.take(i).split(" ").filter(! _.last.isUpper).filter(_.length > 0).toSet | Set("x") else Set("x")
  }
  def readFlags(s: String): Set[String] = {
    val i = s.indexOf("...")
    if (i >= 0) s.take(i).split(" ").filter(_.last.isUpper).filter(_.length > 0).toSet else Set()
  }
  def fixCalls(s: String) = {
    val i = s.indexOf("...")
    if (i >= 0) s.drop(i+3).dropWhile(_ == ' ') else s
  }
  
  val ReplReg = "@[A-Z]+".r
  def fixRepls(ss: Seq[String], mm: Map[String, Set[String]]): Seq[Seq[String]] = {
    val wildcards = ss.flatMap(s => ReplReg.findAllIn(s).map(_.tail)).toSet | Set("A", "CC", "ONE", "ZERO")
    var maps = Seq(Map[String,String]())
    wildcards.map(s => s -> mm(s)).foreach{ wc =>
      val (k,vs) = wc
      val us = vs.toList
      val old = maps
      maps = maps.map(m => (m + (k -> us.head)))
      if (us.tail.nonEmpty) {
        maps = maps ++ us.tail.flatMap(u => old.map(m => (m + (k -> u))))
      }
    }
    maps = maps.filter(_.size > 0)
    if (maps.isEmpty) Seq(ss)
    else maps.map(m => ss.map{ s => 
      val t = ReplReg.replaceAllIn(s, rm => m(rm.toString.tail))
      if (t contains '@') ReplReg.replaceAllIn(t, rm => m(rm.toString.tail)) else t
    })
  }
  
  case class HemiTest(test: String, calls: Set[String], flags: Set[String])
  implicit def testStringToHemiTest(test: String): HemiTest = HemiTest(fixCalls(test), readCalls(test), readFlags(test))
  
  
  def main(args: Array[String]) {
    val laws = new Laws(args contains ("--junit"))
    laws.writeAllTests
  }
}
