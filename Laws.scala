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
    Call("p", "val p = (_i: @A) => _i < i", Inner, Some(Call("", "for (i <- @CD) {", Wraps))),
    Call("n", "for (n <- @CN) {", Wraps),
    Call("m", "for (m <- @CM) {", Wraps),
    Call("a", "for (a <- @CD) {", Wraps),
    Call("b", "for (b <- @CE) {", Wraps),
    Call("x", "val x = @X", Outer),
    Call("y", "val y = @Y", Outer),
    Call("pf", "val pf: PartialFunction[@A,@A] = { case _x if _x in @S => _x+1 }", Outer),
    Call("f", "val f = (_x: Int) = _x+1", Outer),
    Call("z", "for (z <- @CD) {", Wraps),
    Call("op", "val op = (a1: @A, a2: @A) => a1 @OP a2", Outer),
    Call("one", "val one = 1", Outer),
    Call("zero", "val zero = 0", Outer)
  )
  
  val knownRepls = Seq(
    Map(
      "A" -> Set("Int"),
      "CC" -> Set("List[Int]"),
      "X" -> Set("0 to 3", "0 until 0", "0 to 20 by 3", "0 to 64").map("List((" + _ + "): _*)"),
      "Y" -> Set("4 to 8", "0 until 0", "0 to 3", "1 to 20 by 3", "-64 to 0").map("List((" + _ + "): _*)"),
      "CD" -> Set("List(-70, -64, -14, -1, 0, 1, 2, 3, 4, 5, 6, 11, 12, 13, 14, 22, 40, 63, 64, 70)"),
      "CE" -> Set("List(-70, -64, -15, -14, -13, -1, 0, 1, 2, 3, 12, 22, 40, 63, 64, 70)"),
      "CM" -> Set("List(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 39, 40, 41, 64, 65, 66)"),
      "CN" -> Set("List(-2, -1, 0, 1, 2, 3, 4, 5, 6, 11, 12, 13, 14, 22, 40, 63, 64, 70)"),
      "OP" -> Set("+", "*")
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
  def fixRepls(s: String, m: Map[String, String]) =
    ReplReg.replaceAllIn(s, rm => m.get(rm.toString.tail).getOrElse(rm.toString.tail))
  
  case class HemiTest(test: String, needs: Set[String], calls: Set[String])
  implicit def testStringToHemiTest(test: String) = HemiTest(fixCalls(test), readNeeds(test), readCalls(test))
  
  val lawsWithNeeds = Seq[HemiTest](
    "p ... x.`exists`(p) == x.`find`(p).isDefined",
    "p ... x.`forall`(p) implies (x.`isEmpty` || x.`exists`(p))",
    "p ... var y = false; x.`foreach`(xi => y |= p(xi)); y == x.`exists`(p)",
    "x.`toIterator` theSameAs x",
    "x.`toStream` theSameAs x",
    "x.`toTraversable` theSameAs x",
    "z ... x.`aggregate`(z)((b,a) => b, (b1,b2) => b1) == z",
    "z ... x.`aggregate`(z)((b,a) => b, (b1,b2) => b2) == z",
    "", // copyToArray goes here...need some conditions
    "pf ... x.`collectFirst`(pf).isDefined == x.`exists`(pf.isDefinedAt)",
    "val b = new collection.immutable.ArrayBuffer[@A]; x.`copyToBuffer`(b); b.result theSameAs x",
    "p ... x.`count`(p) > 0 == x.`exists`(p)",
    "p ... (x.`count`(p) == x.`size`) == x.`forall`(p)",
    "p ... x.`count`(p) == { var y=0; x.`foreach`(xi => if (p(xi)) y += 1); y }",
    "p ... x.filter(p).`size` == x.`count`(p)", // Filter is part of MonadOps, so we won't test for it!
    "p ... x.filter(p).`forall`(p) == true",
    "", // flatMap goes here, need monad laws
    """a op ... Set(x.`fold`(a)(op), x.`foldLeft`(a)(op), x.`foldRight`(a)(op), x.`/:`(a)(op), x.`:\`(a)(op)).size == 1""",
    "", // map goes here, need monad laws
    "Try{x.`max`}.toOption == Try{x.`reduce`(_ max _)}.toOption",
    "", // maxBy(f)
    "Try{x.`min`}.toOption == Try{x.`reduce`(_ min _)}.toOption",
    "", // minBy(f)
    "x.`nonEmpty` == x.`exists`(_ => true)",
    "one ... x.`product` == x.`fold`(one)(_ * _)",
    "op ... Set(Try{x.`reduce`(op)}.toOption, Try{x.`reduceLeft`(op)}.toOption, Try{x.`reduceRight`(op)}.toOption, x.`reduceLeftOption`(op), x.`reduceRightOption`(op)).size == 1",
    "x.`size` == x.`count`(_ => true)",
    "zero ... x.`sum` == x.`fold`(zero)(_ + _)",
    "x.`to`[@CC] theSameAs x",
    "x.`toArray` theSameAx x",
    "x.`toBuffer` theSameAs x",
    "x.`toIndexedSeq` theSameAs x",
    "x.`toIterable` theSameAs x",
    "x.`toList` theSameAs x",
    "x.`toMap` theSameAs x",
    "x.`toSeq` theSameAs x",
    "x.`toSet` theSameAs x",
    "x.`toVector` theSameAs x",
    "p ... x.`withFilter`(p) theSameAs x.filter(p)",
    "x.`hasNext` == x.`nonEmpty`",
    "x.`hasNext` implies Try{ x.`next` }.isSuccess",
    "y ... x.`++`(y).`size` == x.size + y.size",
    "y ... x.`++`(y).`take`(x.`size`) theSameAs x",
    "y ... x.`++`(y).`drop`(x.`size`) theSameAs y",
    "x.`buffered` theSameAs x",
    "pf ... x.`collect`(pf) theSameAs x.`filter`(pf.isDefinedAt).`map`(pf)",
    "n ... x.`contains`(n) == x.`exists`(_ == n)",
    "x.`corresponds`(x)(_ == _)",
    "y ... (x.`size` != y.size) implies !x.`corresponds`(y)(_ == _)",
    "y ... x.`corresponds`(y)((_,_) => false) implies !x.`nonEmpty` && !y.`nonEmpty`",
    "n ... x.`drop`(n).`size` == (0 max (x.size-(0 max n)))",
    "n ... x.`drop`(n) isPartOf x",
    "p ... val y = x.`dropWhile`(p); !x.`hasNext` || p(x.`next`)",
    "p ... x.`dropWhile`(p) isPartOf x",
    "val (x1,x2) = x.`duplicate`; x1.corresponds(x2)(_ == _) && x1.corresponds(x)(_ == _)",
    "p ... x.`filterNot`(p) theSameAs x.`filter`(xi => !p(xi))",
    "n ... x.`grouped`((1 max n)).map(_.size).`sum` == x.`size`",
    "n m ... var y = 0; (x.`grouped`((1 max n)).`drop`(m).take(1).foreach(y = _.`size`); (y < (1 max n)) implies !x.grouped((1 max n)).drop(m+1).`nonEmpty`",
    "n ... x.`grouped`((1 max n)).flatMap(identity) theSameAs x",
    "x.`isEmpty` == !x.`nonEmpty`",
    "x.`length` == x.`size`",
    "a n ... x.`padTo`(a, n).`size` == (n max x.size)",
    "a n ... x.`padTo`(a, n).`drop`(x.`size`).`forall`(_ == a)",
    "a n ... (n <= x.`size`) implies (x.`padTo`(a, n) theSameAs x)",
    "p ... val (t,f) = x.`partition`(p); (t theSameAs x.`filter`(p)) && (f theSameAs x.`filterNot`(p))",
    "y n m ... x.`patch`(n, y, m).`take`(n) theSameAs x.take(n)",
    "y n m ... x.`patch`(n, y, m).`drop`(n).`take`(y.`size`) theSameAs y",
    "y n m ... x.`patch`(n, y, m).`drop`((0 max n)+y.`size`) theSameAs x.`drop`((0 max n)+(0 max m))",
    "y ... (x `sameElements` y) == (x theSameAs y)",
    "", // scanLeft and scanRight go here
    "n m ... x.`slice`(n, m).`size` == (0 max ((0 max m)-(0 max n)))",
    "n m ... x.`slice`(n, m) theSameAs x.`drop`(n).`take`((0 max m)-(0 max n))",
    "p ... x.`span`(p)._1.`forall`(p)",
    "p ... !x.`span`(p)._2.`take`(1).`exists`(p)",
    "p ... val (x1, x2) = x.`span`(p); val n = x1.`span`(p)._1.`size`; (x1 theSameAs x.`take`(n)) && (x2 theSameAs x.`drop`(n))",
    "n ... x.`take`(n).`size` == ((0 max n) min x.size)",
    "n ... x.`take`(n) isPartOf x",
    "p ... x.`takeWhile`(p).`forall`(p)",
    "p ... x.`takeWhile`(p) isPartOf x",
    "p ... x.`takeWhile`(p).`size` + x.`dropWhile`(p).size == x.size",
    "y ... x.`zip`(y).map(_._1) theSameAs x.take(x.size min y.size)",
    "y ... x.`zip`(y).map(_._2) theSameAs y.take(x.size min y.size)",
    "y a b ... x.`zipAll`(y, a, b).map(_._1) theSameAs x.`padTo`(a, x.size max y.size)",
    "y a b ... x.`zipAll`(y, a, b).map(_._2) theSameAs y.`padTo`(b, x.size max y.size)",
    "x.`zipWithIndex`.map(_._1) theSameAs x",
    "x.`zipWithIndex`.map(_._2).`sameElements`(0 until x.`size`)"
  ).filter(_.test.length > 0)
  
  val lawsAsCode = lawsWithNeeds.map{ ht =>
    val code = Code(Seq(), Seq(), Seq("assert{ " + ht.test + " }"), Seq())
    (code /: ht.calls.toList)((c,x) => 
      knownCalls.find(_.name == x).map(_.satisfy(c)).getOrElse{ println("Could not find "+x); c }
    )
  }
  
  /*
  def explicitGenInt[C[Int] <: TraversableOnce[Int]](ht: HemiTest, factory: String, tname: String, instance: C[Int]): Validated[C[Int]] with Tested[C[Int]] = {
    val test = fixRepls(ht.test, Map("A" -> tname))
    val title = test.filter(_.isLetter)
    if (ht.calls contains "p") {
      GenTest1X(title, factory+"(0,1,2,3)", ht.needs.toSeq, "def p(_i: Int) = _i < i; "+ht.test, "0 to 4", false, instance)
    }
    else {
      GenTest1(title, factory+"(0,1,2,3)", ht.needs.toSeq, ht.test, false, instance)
    }
  }
  
  val testGenerators = lawsWithNeeds.map(ht => explicitGenInt(ht, "List", "Int", List(1)))
  */
  
  /* For consistently orderable collections,
   *   theSame means a.size == b.size and a.foreach(buf += _); i = buf.result.iterator; b.forall(_ == i.next) && !i.hasNext
   * For collections that cannot be consistently ordered,
   *   theSame means a.size == b.size and CountedSet(a) == CountedSet(b)
   */
  
  /* TraversableOnce */
    /*! Why isn't MonadOps a value class? */
    
  /* Iterator */
    /* takeWhile(p).forall(p) == true */
    /* takeWhile(p).size + dropWhile(p).size == size */
    /* i.zip(j).size == i.size min j.size */
    /* i.zip(j).map(_._1).corresponds(i.take(i.zip(j).size))(identity) */
    /* i.zip(j).map(_._2).corresponds(j.take(i.zip(j).size))(identity) */
    /* i.zipAll(j,a,b).size == i.size max j.size */
    /* i.zipAll(j,a,b).map(_._1).corresponds(i.padTo(i.zipAll(j,a,b).size,a))(identity) */
    /* i.zipAll(j,a,b).map(_._2).corresponds(j.padTo(i.zipAll(j,a,b).size,b))(identity) */
    /* i.zipWithIndex.map(_._2).corresponds(0 until i.size)(identity) */
    /* i.zipWithIndex.map(_._2).corresponds(i)(identity) */
    
  /* Traversable */
    /* ++ like Iterator */
    /* a ++: b theSameAs a ++ b */
    /* collect like Iterator */
    /* copyToArray fragment theSameAs self */
    /* drop(n).copyToArray has first n elements the same */
    /* dropWhile(p) theSameAs drop(takeWhile(p).size) */
    /* filterNot like Iterator */
    /* flatten...? */
    /* ... */
    
  /* Invariant: if you are Iterable and have definite size, your iterator is the same size as you. */
  /* Invariant: if you are TraversableOnce and have definite size, you traverse as many elements as your size. */
  
}
