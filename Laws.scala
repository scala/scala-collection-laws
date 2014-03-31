package laws

import scala.util._

object Laws {
  def theSame[A](xs: TraversableOnce[A], ys: TraversableOnce[A], ordered: Boolean = false) = {
    if (ordered) {
      val b = new collection.mutable.ArrayBuffer[A]
      xs.foreach(b += _)
      val i = b.result.iterator
      i.corresponds(ys)(_ == _)
    }
    else {
      val hx, hy = new collection.mutable.HashMap[A, Int]
      xs.foreach(a => hx(a) = hx.getOrElseUpdate(a,0))
      ys.foreach(a => hy(a) = hy.getOrElseUpdate(a,0))
      hx.size == hy.size && hx.forall{ case (k,n) => hy.get(k).exists(_ == n) }
    }
  }
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
  
  def dedollar(s: String) = s
  def sanitized(s: String) = s
  
  trait Validated[C] {
    def instance: C
    def needs: Seq[String]
    lazy val valid = (instance.getClass.getMethods.map(x => dedollar(x.getName)).toSet & needs.toSet) == needs.toSet
  }
  
  trait Tested[C] {
    def title: String
    def pre = List(
      s"//$title",
      s"@Test",
      s"def test_${sanitized(title)} {"
    )
    def post = List(
      s"}"
    )
    def core: Seq[String]
    def wrapped(ss: String*) = (pre ++ ss.map("  " + _) ++ post).mkString("\n")
    lazy val code = wrapped(core: _*)
  }
  
  trait ValidTest[C] extends Validated[C] with Tested[C] {}
  
  sealed trait Pos
  case object Inner extends Pos
  case object Outer extends Pos
  case object Wraps extends Pos
  
  case class Code(pre: Seq[String], lwrap: Seq[String], in: Seq[String], rwrap: Seq[String]) {
    def join = pre ++ lwrap ++ in.map("  "*rwrap.length + _) ++ rwrap
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
  val knownCalls = Seq(
    Call("p", "for (i <- ca; p = (_i: @A) => _i < i) {", Wraps, Some(Call("", "val ca = @CA", Outer))),
    Call("n", "for (n <- cn) {", Wraps, Some(Call("","val cn = @CN",Outer))),
    Call("m", "for (m <- cm) {", Wraps, Some(Call("","val cm = @CM",Outer))),
    Call("a", "for (a <- ca) {", Wraps, Some(Call("","val ca = @CA",Outer))),
    Call("b", "for (b <- cb) {", Wraps, Some(Call("","val cb = @CB",Outer))),
    Call("x", "val x = @X", Outer),
    Call("y", "val ys = @YS", Outer, Some(Call("", "for (y <- ys) {", Wraps))),
    Call("pf", "for (i <- ca; pf = @PF) {", Wraps, Some(Call("", "val ca = @CA", Outer))),
    Call("f", "val f = @F", Outer),
    Call("z", "for (z <- ca) {", Wraps, Some(Call("","val ca = @CA",Outer))),
    Call("op", "val op = (a1: @A, a2: @A) => a1 @OP a2", Outer),
    Call("one", "val one = @ONE", Outer),
    Call("zero", "val zero = @ZERO", Outer)
  )
  
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
        val tame = subbee.map{ splitArrow2 }.toMap + ("A" -> a) + ("CC" -> cc)
        (tame ++ wildMap.filterKeys(k => !tame.contains(k)), substs)
      }
    }
    val elaborated = wildInserted.mapValues(_.map{ case (tame, substs) => (tame /: substs)((t,s) => t.mapValues(s.sub)).toMap })
    elaborated.mapValues(_.map(_.mapValues(s => smartSplit(s).toSet)))
  }
  
  val knownRepls = readReplacementsFile("replacements.tests")
  
  val NeedReg = "`[^`]+`".r
  def readNeeds(s: String) = NeedReg.findAllIn(s).map(x => x.slice(1,x.length-1)).toSet
  
  def readCalls(s: String) = {
    val i = s.indexOf("...")
    if (i >= 0) s.take(i).split(" ").filter(! _.last.isUpper).filter(_.length > 0).toSet | Set("x") else Set("x")
  }
  def readFlags(s: String) = {
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
  
  case class HemiTest(test: String, needs: Set[String], calls: Set[String])
  implicit def testStringToHemiTest(test: String) = HemiTest(fixCalls(test), readNeeds(test), readCalls(test))
  
  val lawsWithNeeds = {
    val src = scala.io.Source.fromFile("single-line.tests")
    try { 
      src.getLines.map(_.trim).filter(_.length > 0).
        filterNot(_ startsWith "//").map(_.split("//").head).
        map(line => (line : HemiTest)).toVector 
    }
    finally { src.close }
  }
  
  val lawsAsWildCode = lawsWithNeeds.map{ ht =>
    val code = Code(Seq(), Seq(), Seq("assert{ " + ht.test + " }"), Seq())
    (code /: ht.calls.toList)((c,x) => 
      knownCalls.find(_.name == x).map(_.satisfy(c)).getOrElse{ println("Could not find "+x); c }
    )
  }
  
  val lawsAsCode = knownRepls.mapValues(_.map{ rep =>
    lawsAsWildCode.groupBy(_.pre).toList.map{ case (pre, codes) =>
      val lines = codes.groupBy(_.lwrap).toList.flatMap{ case (lwrap, somecodes) =>
        somecodes.head.lwrap ++ somecodes.flatMap(c => c.join.drop(c.pre.length + c.lwrap.length).dropRight(c.rwrap.length)) ++ somecodes.head.rwrap
      }
      fixRepls(pre ++ lines, rep)
    }
  })
}
