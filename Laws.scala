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
    Call("y", "val y = @Y", Outer),
    Call("pf", "for (i <- ca; pf = { case _i if _i < i => _i+1 }: PartialFunction[@A,@A]) {", Wraps, Some(Call("", "val cd = @CA", Outer))),
    Call("f", "val f = (_x: Int) = _x+1", Outer),
    Call("z", "for (z <- ca) {", Wraps, Some(Call("","val ca = @CA",Outer))),
    Call("op", "val op = (a1: @A, a2: @A) => a1 @OP a2", Outer),
    Call("one", "val one = @ONE", Outer),
    Call("zero", "val zero = @ZERO", Outer)
  )
  
  val knownRepls = Seq(
    Map(
      "A" -> Set("Int"),
      "CC" -> Set("List[Int]"),
      "X" -> Set("0 to 3", "0 until 0", "0 to 20 by 3", "0 to 64").map("List((" + _ + "): _*)"),
      "Y" -> Set("4 to 8", "0 until 0", "0 to 3", "1 to 20 by 3", "-64 to 0", "2 to 3", "0 to 1").map("List((" + _ + "): _*)"),
      "CA" -> Set("List(-70, -64, -14, -1, 0, 1, 2, 3, 4, 5, 6, 11, 12, 13, 14, 22, 40, 63, 64, 70)"),
      "CB" -> Set("List(-70, -64, -15, -14, -13, -1, 0, 1, 2, 3, 12, 22, 40, 63, 64, 70)"),
      "CM" -> Set("List(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 39, 40, 41, 64, 65, 66)"),
      "CN" -> Set("List(-2, -1, 0, 1, 2, 3, 4, 5, 6, 11, 12, 13, 14, 22, 40, 63, 64, 70)"),
      "OP" -> Set("+", "*"),
      "ONE" -> Set("1"),
      "ZERO" -> Set("0")
    )
  )
  
  val NeedReg = "`[^`]+`".r
  def readNeeds(s: String) = NeedReg.findAllIn(s).map(x => x.slice(1,x.length-1)).toSet
  
  def readCalls(s: String) = {
    val i = s.indexOf("...")
    if (i >= 0) s.take(i).split(" ").filter(_.length > 0).toSet | Set("x") else Set("x")
  }
  def fixCalls(s: String) = {
    val i = s.indexOf("...")
    if (i >= 0) s.drop(i+3).dropWhile(_ == ' ') else s
  }
  
  val ReplReg = "@[A-Z]+".r
  def fixRepls(ss: Seq[String], mm: Map[String, Set[String]]): Seq[Seq[String]] = {
    val wildcards = ss.flatMap(s => ReplReg.findAllIn(s).map(_.tail)).toSet
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
    else maps.map(m => ss.map(s => ReplReg.replaceAllIn(s, rm => m(rm.toString.tail))))
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
  
  val lawsAsCode = lawsAsWildCode.groupBy(_.pre).toList.map{ case (pre, codes) =>
    val lines = codes.groupBy(_.lwrap).toList.flatMap{ case (lwrap, somecodes) =>
      somecodes.head.lwrap ++ somecodes.flatMap(c => c.join.drop(c.pre.length + c.lwrap.length).dropRight(c.rwrap.length)) ++ somecodes.head.rwrap
    }
    fixRepls(pre ++ lines, knownRepls.head)
  }
}
