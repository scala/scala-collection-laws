package laws

/** An individual law that should be followed by (some) collections. */
case class Law(name: String, tags: Tags, code: String, disabled: Boolean = false)(implicit file: sourcecode.File, line: sourcecode.Line) {
  /** Constructor with code only (no name or tags) */
  def this(code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = 
    this("", Tags.empty, code)(file, line)

  /** Constructor with no tags (but with name) */
  def this(name: String, code: String)(implicit file: sourcecode.File, line: sourcecode.Line) =
    this(name, Tags.empty, code)(file, line)

  /** Constructor with tags (but no name) */
  def this(tags: Tags, code: String)(implicit file: sourcecode.File, line: sourcecode.Line) =
    this("", tags, code)(file, line)

  /** Extracts the named methods within the code block of this law */
  private[this] def findMyMethods: Either[Law.FormatErr, Set[String]] = {
    val b = Array.newBuilder[String]
    var i = 0
    while (i >= 0 && i < code.length) {
      i = code.indexOf('`', i)
      if (i >= 0) {
        val j = code.indexOf('`', i+1)
        if (j > i+1) b += code.substring(i+1, j)
        else return Left(Law.FormatErr("Unclosed method quotes", code, i, code.substring(i)))
        i = j+1
      }
    }
    Right(b.result.toSet)
  }

  /** Methods in the law that are backtick-quoted, indicating that the collection should only be used if it has those methods */
  val methods = findMyMethods

  /** Function that checks whether a set of methods contains the methods needed to run this law */
  val checker = findMyMethods.fold(_ => MethodChecker.empty, s => new MethodChecker(s))

  val cleanCode = findMyMethods.
    map(ms => ms.foldLeft(code)((c, m) => c.replace(f"`$m`": CharSequence, m: CharSequence))).
    getOrElse(code)

  /** The line on which this law was defined; we assume for now that they're all from the same file */
  val lineNumber = line.value

  override def toString =
    (if (name.isEmpty) "" else f"// $name\n") +
    code +
    (if (tags.isEmpty) "" else "\n// # " + tags.toString) +
    "\n// @ " + Sourced.local(file, line) + "\n"
}
object Law {
  /** FormatErr represents an error in the formatting of a collection law.  Presently,
    * the only thing that can go wrong is an unclosed method name (the checked methods
    * should be enclosed in backticks).
    */
  case class FormatErr(description: String, context: String, position: Int, focus: String) {
    override def toString = f"$description.  At $position found $focus.  In $context"
  }

  def apply(code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = new Law(code)(file, line)
  def apply(name: String, code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = new Law(name, code)(file, line)
  def apply(tags: Tags, code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = new Law(tags, code)(file, line)  
}

/** Specifies all the individual laws that should hold for collections.
  *
  * Flags are used to indicate which laws are appropriate for which collections (if they are not universally applicable).
  *
  * Flags do not need to be used to select collections on the basis of available methods; just quote the methods
  * in backticks and the code generator will figure it out.
  *
  * Do NOT ignore warnings about ignored values!  If you forget `.law` on the end of a code string, you
  * will get one of these.  All the law generation/registration extension methods return `Unit` so'
  * that these warnings are meaningful.
  */
object Laws {
  import Flag._
  import Tags.Implicits._

  /** Implements runtime filters for various values that tests may use.
    * 
    * Make sure you filter out, at runtime, only things where other alternatives exist!
    * For instance, if you want `n` to be greater than 3, you should first filter to make sure
    * that `xsize` is greater than 3; otherwise a smaller collection will be chosen, the
    * filtering will reject everything, and the algorithm will not explore any further because
    * that collection size was never used.
    */
  object Filt {
    def n(q: Int => Boolean)  = (ti: TestInfo) => if (q(ti.num.values.n))  None else Some(Outcome.Skip.n)
    def nn(q: Int => Boolean) = (ti: TestInfo) => if (q(ti.num.values.nn)) None else Some(Outcome.Skip.nn)
    def m(q: Int => Boolean)  = (ti: TestInfo) => if (q(ti.num.values.m))  None else Some(Outcome.Skip.m)
    def mm(q: Int => Boolean) = (ti: TestInfo) => if (q(ti.num.values.mm)) None else Some(Outcome.Skip.mm)
    def r(q: Int => Boolean)  = (ti: TestInfo) => if (q(ti.num.values.r))  None else Some(Outcome.Skip.r)

    def f(q: (_ ===> _) => Boolean)       = (ti: TestInfo) => if (q(ti.oper.values.f))  None else Some(Outcome.Skip.f)
    def g(q: (_ ===> _) => Boolean)       = (ti: TestInfo) => if (q(ti.oper.values.g))  None else Some(Outcome.Skip.g)
    def op(q: OpFn[_] => Boolean)         = (ti: TestInfo) => if (q(ti.oper.values.op)) None else Some(Outcome.Skip.op)
    def pf(q: ParFn[_] => Boolean)        = (ti: TestInfo) => if (q(ti.oper.values.pf)) None else Some(Outcome.Skip.pf)
    def q(q: (_ ===> Boolean) => Boolean) = (ti: TestInfo) => if (q(ti.oper.values.p))  None else Some(Outcome.Skip.p)

    def xsize(q: Int => Boolean)      = (ti: TestInfo) => if (q(ti.inst.values.xsize)) None else Some(Outcome.Skip.x)
    def ysize(q: Int => Boolean) = (ti: TestInfo) => if (q(ti.inst.values.ysize)) None else Some(Outcome.Skip.y)

    val zero  = (ti: TestInfo) => if (ti.oper.values.op.zero.isDefined)            None else Some(Outcome.Skip.op)
    val assoc = (ti: TestInfo) => if (ti.oper.values.op.assoc == OpFn.Associative) None else Some(Outcome.Skip.op)
    val sym   = (ti: TestInfo) => if (ti.oper.values.op.sym == OpFn.Symmetric)     None else Some(Outcome.Skip.op)
  }

  private val b = Array.newBuilder[Law]

  /** Implicits used to make the definition of laws as low-boilerplate as possible */
  implicit class LawMaker(rawText: String)(implicit file: sourcecode.File, line: sourcecode.Line) {
    /** Used to force implicit resolution of this class if it's otherwise ambiguous */
    def make: this.type = this

    /** The text used for code generation (slightly cleaned up from the string literal) */
    lazy val text: String = { 
      var i = 0;
      while (i < rawText.length && (rawText(i) == '\n' || rawText(i) == '\r')) i += 1;
      var j = rawText.length - 1;
      while (j > 0 && (rawText(j) == '\n' || rawText(j) == '\r')) j -= 1;
      if (i > j) rawText
      else if (i > 0 && j < rawText.length-1) rawText.substring(i, j+1)
      else if (i > 0) rawText.substring(i)
      else if (j < rawText.length-1) rawText.substring(0, j+1)
      else rawText
    }

    /** This law should not presently be used */
    def skip: Unit = {
      b += new Law("", Tags.empty, text, disabled = true)
    }

    /** A law with no tags */
    def law: Unit = { 
      b += new Law(text)
    }

    /** A law with tags */
    def law(tag: Tags.Taggish, more: Tags.Taggish*): Unit = {
      b += new Law(Tags(tag, more: _*), text)
    }

    /** A named law with no tags */
    def named(name: String): Unit = {
      b += new Law(name, text)
    }

    /** A named law with tags */
    def named(name: String, tag: Tags.Taggish, more: Tags.Taggish*): Unit = {
      b += new Law(name, Tags(tag, more: _*), text)
    }
  }

//////////////////////////////
// Individual laws begin.   // 
// Indentation removed for  //
// greater clarity but this //
// is still inside the      //
// Laws object!             //
//////////////////////////////

/*
*****************************************************************************
A guide to tags

Various string tags are used to indicate properties that hold for only
certain types of colletions.  These include:

selfs - only holds for collections that can maintain the same type after
        elements are dropped or scrambled (but have the same type)
seq - only holds for collections that have a well-defined consistent order
set - only holds for collections that remove duplicates
*****************************************************************************
*/

/******** Basic capabilities ********/

"x.`map`(f) sameAs { val y = collection.mutable.ArrayBuffer.empty[A]; x.foreach(xi => y += f(xi)); y }".law(SEQ)

"x.`map`(f) sameAs { val y = collection.mutable.HashSet.empty[A]; x.foreach(y += _); y.map(f) }".law(SET, BITSET.!)

"x.`map`(bitset_f) sameAs { val y = collection.mutable.HashSet.empty[A]; x.foreach(y += _); y.map(bitset_f) }".law(BITSET)

"x sameType x.`map`(f)".law(ORDERLY.!, SPECTYPE.!, CPMH_TYPE_11449.!)

"""{
  val flat = x.`flatMap`(xi => y.toList.take(intFrom(xi)))
  val ref = collection.mutable.ArrayBuffer.empty[A]
  x.foreach(xi => ref ++= y.toArray.take(intFrom(xi)))
  flat sameAs ref
}""".law(SEQ)

"""{
  val flat = x.`flatMap`(xi => y.toList.take(intFrom(xi)))
  val ref = collection.mutable.HashSet.empty[A]
  x.foreach(xi => ref ++= y.toArray.take(intFrom(xi)))
  flat sameAs ref
}""".law(SET)

"x sameType x.`flatMap`(xi => y.toList.take(intFrom(xi)%3))".law(ORDERLY.!, SPECTYPE.!, CPMH_TYPE_11449.!)

"x.`exists`(p) == x.`find`(p).isDefined".law

"x.`forall`(p) implies (x.`isEmpty` || x.`exists`(p))".law

"{ var y = false; x.`foreach`(xi => y |= p(xi)); y == x.`exists`(p) }".law

"x.`toIterator` sameAs x".law

"x.`toStream` sameAs x".law

"x.`toTraversable` sameAs x".law

/******** String generation ********/

"x.`mkString` == { val sb = new StringBuilder; x.foreach(sb ++= _.toString); sb.result }".law(SEQ)

"x.`mkString` == { val sb = new StringBuilder; x.`addString`(sb); sb.result }".law

"""x.`mkString` == x.mkString("", "", "")""".law

"""x.`mkString`("!") == x.mkString("", "!", "")""".law

"""{
  (x.`size` == 0) || (
    x.`mkString`("!", "@", "#") == {
      val sb = new StringBuilder
      sb ++= "!"
      x.foreach(sb ++= _.toString + "@")
      sb.result.dropRight(1) + "#"
    }
  )
}""".law(SEQ)

"""x.`mkString`("*") == { val sb = new StringBuilder; x.`addString`(sb, "*"); sb.result }""".law(SEQ)

"""x.`mkString`("[", "|", "]") == { val sb = new StringBuilder; x.`addString`(sb, "[", "|", "]"); sb.result }""".law(SEQ)


/******** Everything else (roughly alphabetical) ********/

"x.`aggregate`(zero)((b,a) => b, (b1,b2) => b1) == zero".law(Filt.zero)

"x.`aggregate`(zero)((b,a) => b, (b1,b2) => b2) == zero".law(Filt.zero)

"x.`array`.take(x.`size`).toVector == x.toVector".law

"""
val arr = new Array[A](nn)
x.`copyToArray`(arr, n, m)
(x.toList zip arr.drop(n).take(m)).forall{ case (x, y) => x == y }
""".law(SET.!, MAP.!, Filt.xsize(_ > 0))


"""
val arr = new Array[A](nn)
x.`copyToArray`(arr, n, m)
val c = arr.drop(n).take(m min x.`size`)
val cs = collectionFrom(c)
c.size == cs.size && (cs subsetOf x.toSet)
""".law(SET, Filt.xsize(_ > 0))

"""
val arr = new Array[A](nn)
val x0 = x
x0.`copyToArray`(arr, n, m)
(n until ((n+m) min x.`size`)).forall(i => x0.get(arr(i)._1).exists(_ == arr(i)._2))
""".law(MAP, Filt.xsize(_ > 0))


"x.`collectFirst`(pf).isDefined == x.`exists`(pf.isDefinedAt)".law

"x.`collectFirst`(pf) == x.`dropWhile`(xi => !pf.isDefinedAt(xi)).`headOption`.map(pf)".law(SEQ)

"""
val b = new collection.mutable.ArrayBuffer[A]
x.`copyToBuffer`(b)
b sameAs x
""".law

"x sameAs x.`companion`(x.toList: _*)".law

"x.`count`(p) > 0 == x.`exists`(p)".law

"(x.`count`(p) == x.`size`) == x.`forall`(p)".law

"x.`count`(p) == { var k=0; x.`foreach`(xi => if (p(xi)) k += 1); k }".law

"x.`empty`.`isEmpty`".law

"x sameType x.`empty`".law

"x.filter(p).`size` == x.`count`(p)".law

"x.filter(p).`forall`(p) == true".law

"x sameType x.`filter`(p)".law

"""
val oneStep = x.flatMap(xi => y.toList.take(intFrom(xi) % 3 max 0))
val twoStep = x.map(xi => y.toList.take(intFrom(xi) % 3 max 0)).`flatten`
oneStep sameAs twoStep
""".law(MAP.!, ORDERLY.!, BITSET_MAP_AMBIG.!)

"""
val oneStep = x.flatMap(xi => y.toList.take(intFrom(xi) % 3 max 0))
val twoStep = x.`unsorted`.map(xi => y.toList.take(intFrom(xi) % 3 max 0)).`flatten`
oneStep sameAs twoStep
""".law(MAP.!, ORDERLY)


"""
val oneStep = x.flatMap(xi => y.toList.take(intFrom(xi) % 3 max 0))
val twoStep = collectionFrom(x.toArray.map(xi => y.toList.take(intFrom(xi) % 3 max 0).toArray).`flatten`)
oneStep sameAs twoStep
""".law(MAP.!)

"x.`foldLeft`(a)(op) == x.`foldRight`(a)(op)".law(Filt.sym)

"x.`foldLeft`(a)(op) == x.`/:`(a)(op)".law

"""x.`foldRight`(a)(op) == x.`:\`(a)(op)""".law

"x.`isDefinedAt`(r) == (0 <= r && r < x.`size`)".law(SEQ)

"x.`isDefinedAt`(a._1) == x.`get`(a._1).isDefined".law(MAP)

"x.`isTraversableAgain`".law(ONCE.!)

"!x.`isTraversableAgain`".law(ONCE)

"tryO{x.`max`} == tryO{ x.`reduce`(maxOf) }".law

"tryO{x.`maxBy`(f)} == tryO{ val fx = x.map(f).`max`; x.find(xi => f(xi)==fx).get }".law(BITSET.!)

"tryO{x.`maxBy`(bitset_f)} == tryO{ val fx = x.map(bitset_f).`max`; x.find(xi => bitset_f(xi)==fx).get }".law(BITSET)

"tryO{ x.`max` } == x.`maxOption`".law

"tryO{ x.`maxBy`(f) } == x.`maxByOption`(f)".law(BITSET.!)

"tryO{ x.`maxBy`(bitset_f) } == x.`maxByOption`(bitset_f)".law(BITSET)

"tryO{x.`min`} == tryO{ x.`reduce`(minOf) }".law

"tryO{x.`minBy`(f)} == tryO{ val fx = x.map(f).`min`; x.find(xi => f(xi)==fx).get }".law(BITSET.!)

"tryO{x.`minBy`(bitset_f)} == tryO{ val fx = x.map(bitset_f).`min`; x.find(xi => bitset_f(xi)==fx).get }".law(BITSET)

"tryO{ x.`min` } == x.`minOption`".law

"tryO{ x.`minBy`(f) } == x.`minByOption`(f)".law(BITSET.!)

"tryO{ x.`minBy`(bitset_f) } == x.`minByOption`(bitset_f)".law(BITSET)



"x.`nonEmpty` == x.`exists`(_ => true)".law

"x.`product` == x.`fold`(1)(_ * _)".law(INT)

"x.`sum` == x.`fold`(0)(_ + _)".law(INT)

"""
Set(
  tryO{x.`reduce`(op)}, tryO{x.`reduceLeft`(op)}, tryO{x.`reduceRight`(op)},
  x.`reduceOption`(op), x.`reduceLeftOption`(op), x.`reduceRightOption`(op)
).size == 1
""".law(Filt.assoc)


"x.`size` == x.`count`(_ => true)".law

"math.signum(x.`sizeCompare`(y)) == math.signum(x.`size`.compare(y.`size`))".law(ARRAY.!)
"math.signum(x.`sizeCompare`(y.`size`)) == math.signum(x.`size`.compare(y.`size`))".law

"x.`sizeIs` < m == x.`size` < m".law

"x.`sizeIs` > m == x.`size` > m".law

"(x.`sizeIs` == m) == (x.`size` == m)".law

"x.`sizeIs` < m == x.`lengthIs` < m".law

"x.`sizeIs` > m == x.`lengthIs` > m".law

"(x.`sizeIs` == m) == (x.`lengthIs` == m)".law


"x sameAs x.`to`(List)".law

"x.`toArray` sameAs x".law

"x.`toBuffer` sameAs x".law

"x.`toIndexedSeq` sameAs x".law

"x.`toIterable` sameAs x".law

"x.`toList` sameAs x".law

"""
val tomap = x.map(xi => (xi,xi)).`toMap`
val canon = { 
  val hm = new collection.mutable.HashMap[A,A]
  x.foreach(xi => hm += xi -> xi)
  hm
}
tomap samePieces canon 
""".law(BITSET_MAP_AMBIG.!)

"x.`toSeq` sameAs x".law

"x.`toSet` partOf x".law

"x.`toVector` sameAs x".law

"""
val c = new collection.mutable.ArrayBuffer[A];
x.`withFilter`(p).foreach(c += _);
c sameAs x.filter(p)""".law(SET.!, MAP.!)

"""
val c = new collection.mutable.HashSet[A];
x.`withFilter`(p).foreach(c += _);
c sameAs x.filter(p)""".law(SET)

"x.`hasNext` == x.`nonEmpty`".law

"x.`hasNext` implies tryO{ x.`next` }.isDefined".law

"x.`nextOption` == tryO{ x.`next` }".law

"x.`++`(y).`size` == x.size + y.size".law(SEQ)

"x.`++`(y).`size` <= x.size + y.size".law(SEQ.!)

"x.`++`(y).`size` == x.size + y.filter(yi => !x.`contains`(yi)).size".law(SET)

"x.`++`(y).`size` == x.size + y.filter(yi => !x.`contains`(yi._1)).size".law(MAP)

"(x.`++`(y)).`take`(x.`size`) sameAs x".law(SEQ)

"(x.`++`(y)).`drop`(x.`size`) sameAs y".law(SEQ)

"x.`++`(y) sameAs (x.toSeq ++ y.toSeq).toMap".law(MAP)

"x.`++`(y) sameAs (x.toSeq ++ y.toSeq).toSet".law(SET)

"x.`--`(y).`size` >= x.size - y.size".law(MAP.!)


"x.`--`(y.map(_._1)).`size` >= x.size - y.size".law(MAP)

"(x.`--`(y)) partOf x".law(MAP.!)

"x.`--`(y.map(_._1)) partOf x".law(MAP)

"""
val List(q, qq, qqq) = List(x, y, x.`--`(y)).map(_.groupBy(identity).mapValues(_.size));
qqq.forall{ case (k,v) => v == math.max(0, q.getOrElse(k,0) - qq.getOrElse(k,0)) }""".law(MAP.!)

"""
val yy = y.map(_._1).toSet
val xx = x.`--`(yy)
x.forall{ case (k, v) =>
  (yy(k) && ! xx.contains(k)) ^ (xx.get(k) == Some(v))
}
""".law(MAP)

"x sameType x.`++`(y)".law(ACC_SPEC.!, CPMH_TYPE_11449.!)

"x.`buffered` sameAs x".law

"x.`collect`(pf) sameAs x.`filter`(pf.isDefinedAt).`map`(pf)".law(BITSET_MAP_AMBIG.!)

"x sameType x.`collect`(pf)".law(ORDERLY.!, BITSET_MAP_AMBIG.!, SPECTYPE.!, CPMH_TYPE_11449.!)

"x.`contains`(a) == x.`exists`(_ == a)".law(MAP.!)

"x.`contains`(a._1) == x.`exists`(_._1 == a._1)".law(MAP)

"x.`corresponds`(x)(_ == _)".law

"(x.`size` != y.size) implies !x.`corresponds`(y)(_ == _)".law

"x.`corresponds`(y)((_,_) => false) implies !x.`nonEmpty` && !y.`nonEmpty`".law

"x.`drop`(n).`size` == (0 max (x.size-(0 max n)))".law

"x.`drop`(n) partOf x".law

"x sameType x.`drop`(n)".law

"""
val c = x.`dropWhile`(p);
c.`take`(1).toList match { 
  case Nil => true
  case List(xi) => !p(xi)
  case _ => false
}""".law(SEQ)

"""
val y = x.`dropWhile`(p)
y.nonEmpty implies y.exists(yi => !p(yi))
""".law

"""x.`dropWhile`(p) partOf x""".law

"x sameType x.`dropWhile`(p)".law
"""
val (x1,x2) = x.`duplicate`
x1.`corresponds`(x)(_ == _) && x2.corresponds(x)(_ == _)
""".law

"""
val (x1,x2) = x.`duplicate`
(x sameType x1) && (x1 sameType x2)
""".law

"x.`filterNot`(p) sameAs x.`filter`(xi => !p(xi))".law

"x sameType x.`filterNot`(p)".law

"x.`getOrElse`(a._1, a._2) == x.`get`(a._1).getOrElse(a._2)".law(MAP)

"n <= 0 || x.`grouped`(n).map(_.size).`sum` == x.`size`".law

"""n <= 0 || m < 0 || {
  var y = 0; x.`grouped`(n).`drop`(m).take(1).map(_.`size`).foreach(y = _); (y < n) implies !x.grouped(n).drop(m+1).`nonEmpty`
}""".law

"x.`grouped`((1 max n)).flatMap(xi => xi) sameAs x".law

"x.`hasDefiniteSize`".law(INDEF.!, Filt.xsize(_ > 0))

"!x.`hasDefiniteSize`".law(INDEF, Filt.xsize(_ > 0))

"x.`isEmpty` == !x.`nonEmpty`".law

"x sameType x.`iterableFactory`.apply(a)".law(SPECTYPE.!, MAP.!, ORDERLY.!)

"x.`iterableFactory`.apply(a).`iterator`.toList == List(a)".law

"x.`length` == x.`size`".law

"x.`mapResult`(_.`map`(f)).`addOne`(a).`result` sameAs x.`addOne`(a).`result`.`map`(f)".law(BITSET.!)

"x.`mapResult`(_.`map`(bitset_f)).`addOne`(a).`result` sameAs x.`addOne`(a).`result`.`map`(bitset_f)".law(BITSET)

"x.`padTo`(n, a).`size` == (n max x.size)".law

"x.`padTo`(n, a).`drop`(x.`size`).`forall`(_ == a)".law

"(n <= x.`size`) implies (x.`padTo`(n, a) sameAs x)".law

"x sameType x.`padTo`(n,a)".law(SPECTYPE.!)

"""
val (t,f) = x.`partition`(p)
(t sameAs x.`filter`(p)) && (f sameAs x.`filterNot`(p))
""".law

"""
val (t,f) = x.`partition`(p)
(t sameType f) && (t sameType x)
""".law

"""
val (l, r) = x.`partitionMap`(xi => if (p(xi)) Left(f(xi)) else Right(xi))
val ll = x.`filter`(p).`map`(f)
val rr = x.`filterNot`(p)
(l sameAs ll) && (r sameAs rr)
""".law(BITSET.!)

"""
val (l, r) = x.`partitionMap`(xi => if (p(xi)) Left(bitset_f(xi)) else Right(xi))
val ll = x.`filter`(p).`map`(bitset_f)
val rr = x.`filterNot`(p)
(l sameAs ll) && (r sameAs rr)
""".law(BITSET)

"""
val (l, r) = x.`partitionMap`(xi => if (p(xi)) Left(f(xi)) else Right(xi))
(x sameType l) && (x sameType r)
""".law(MAP.!, SPECTYPE.!, ORDERLY.!)

"(n <= x.`size`) implies (x.`patch`(n, y, m).`take`(n) sameAs x.take(n))".law

"(n <= x.`size`) implies (x.`patch`(n, y, m).`drop`(n).`take`(y.`size`) sameAs y)".law

"(n <= x.`size`) implies (x.`patch`(n, y, m).`drop`((0 max n)+y.`size`) sameAs x.`drop`((0 max n)+(0 max m)))".law

"(x `sameElements` y) == (x sameAs y)".law

"n < 0 || m >= x.size || { x.`slice`(n, m).`size` == (0 max ((0 max m)-n)) }".law

"n < 0 || m >= x.size || { x.`slice`(n, m) sameAs x.`drop`(n).`take`((0 max m)-n) }".law(SET.!)

"n < 0 || m >= x.size || (x sameType x.`slice`(n, m))".law

"x.`span`(p)._1.`forall`(p)".law

"(x.span(p)._2.`size`) > 0 implies !x.`span`(p)._2.`take`(1).`exists`(p)".law(SEQ)

"(x.span(p)._2.`size`) > 0 implies !x.`span`(p)._2.`forall`(p)".law

"val (x1, x2) = x.`span`(p); val n = x.`span`(p)._1.`size`; (x1 sameAs x.`take`(n)) && (x2 sameAs x.`drop`(n))".law

"""
val (x1, x2) = x.`span`(p)
(x1 sameType x2) && (x sameType x1)
""".law

"x.`take`(n).`size` == ((0 max n) min x.size)".law

"x.`take`(n) partOf x".law

"x sameType x.`take`(n)".law

"x.`takeWhile`(p).`forall`(p)".law

"x.`takeWhile`(p) partOf x".law

"x.`takeWhile`(p).`size` + x.`dropWhile`(p).size == x.size".law

"x sameType x.`takeWhile`(p)".law

"x.`zip`(y).map(_._1) sameAs x.take(x.size min y.size)".law(BITSET_ZIP_AMBIG.!)

"x.`zip`(y).map(_._2) sameAs y.take(x.size min y.size)".law(BITSET_ZIP_AMBIG.!)

"x sameType x.`zip`(y).map(_._1)".law(MAP.!, ORDERLY.!, SPECTYPE.!)

"x.`zipAll`(y, a, f(a)).map(_._1) sameAs x.`padTo`(x.`size` max y.size, a)".law

"x.`size` < y.size implies x.`zipAll`(y, a, f(a)).map(_._1).`exists`(_ == a)".law

"x.`zipAll`(y, a, f(a)).map(_._2) sameAs y.`padTo`(x.`size` max y.size, f(a))".law

"x.`size` > y.size implies x.`zipAll`(y, a, f(a)).map(_._2).`exists`(_ == f(a))".law

"""
val z = x.`zipAll`(y, a, f(a))
val x0 = x.`++`(Vector.fill((y.size - x.`size`) max 0)(a))
val y0 = y.`++`(Vector.fill((x.`size` - y.size) max 0)(f(a)))
z sameAs x0.`zip`(y0)
""".law(SEQ)

"""
val xlen = x.`size`
val ylen = y.`size`
val zip =
  if (xlen == ylen) x.zip(y)
  else if (xlen < ylen) (x.`++`(Iterator.continually(a).take(ylen-xlen))).zip(y)
  else                   x.`zip`(y ++ Iterator.continually(f(a)).take(xlen-ylen))
x.`zipAll`(y, a, f(a)) sameAs zip
""".law(SEQ)

"x sameType x.`zipAll`(y, a, f(a)).map(_._1)".law(MAP.!, ORDERLY.!, SPECTYPE.!)

"x.`zipWithIndex`.map(_._1) sameAs x".law

"x.`zipWithIndex`.map(_._2) sameAs (0 until x.`size`)".law

"x sameType x.`zipWithIndex`.map(_._1)".law(MAP.!, ORDERLY.!, SPECTYPE.!)

/* 
// The :++ method does not actually exist
"x.`:++`(y) sameAs x.`++`(y)".law
*/

"x.`++:`(y) sameAs y.`++`(x)".law(SEQ)

"(x.`++:`(y) samePieces y.`++`(x)) || (x.`++:`(y) samePieces x.`++`(y))".law(SEQ.!)

"x.`++:`(y) sameType y.`++`(x)".law(SEQ)

"x.`::`(a).`size` == x.size+1".law

"x.`::`(a).`head` == a".law

"x sameType x.`::`(a)".law

"x.`+:`(a).`size` == x.size+1".law

"x.`+:`(a).`head` == a".law

"x sameType x.`+:`(a)".law(SPECTYPE.!)

"x.`:+`(a).`size` == x.size+1".law

"x.`:+`(a).`last` == a".law

"x sameType x.`:+`(a)".law(SPECTYPE.!)

"val s = x.`+`(a).`size` - x.size; 0 <= s && s <= 1".law

"x.`+`(a).`contains`(a)".law(MAP.!)

"x.`+`(a).`contains`(a._1)".law(MAP)

"x sameType x.`+`(a)".law(CPMH_TYPE_11449.!)

"x.`:::`(y) sameAs y.`++`(x)".law

"x sameType x.`:::`(y)".law

"x.`apply`(n) == x.`drop`(n).`head`".law(SEQ, Filt.xsize(_ > 0))

"x.`apply`(a) == x.`contains`(a)".law(SET)

"tryO{ x.`apply`(a._1) } == x.`get`(a._1)".law(MAP)

"""
val n = x.`combinations`(r).size;
((r > x.size) implies (n == 0)) && x.combinations(r).toSet.size == n
""".law(Filt.xsize(_ <= 8))

"""r == 0 || r > x.size || { 
  val s = x.toVector
  x.`combinations`(r).forall(_ partOf s)
}""".law(Filt.xsize(_ <= 8))

"""
x.`containsSlice`(y) implies (
  y.`size` == 0 ||
  ( Iterator.from(0).
    map(x.`drop`).
    takeWhile(_.`nonEmpty`).
    exists(_.`take`(y.`size`) sameAs y)
  )
)
""".law

"""
val List(xg,yg,xdyg) =
  List(x,y,x.`diff`(y)).map(_.`groupBy`(identity).mapValues(_.size).toMap)

xg.forall{ case (k, n) => 
  val m = n - yg.getOrElse(k,0)
  (m > 0) implies (xdyg.getOrElse(k,0) == m)
}
""".law

"""
val s = x.`toSet`
x.`distinct`.`size` == s.size && x.forall(s)
""".law

"x.`dropRight`(nn) sameAs x.`take`(x.`size` - math.max(0, nn))".law(SET.!)

"x sameType x.`reverse`".law

"x.`endsWith`(y) == (x.`drop`(math.max(0, x.`size`-y.size)) sameAs y)".law

"x.`groupBy`(g).keySet == x.map(g).toSet".law(BITSET_MAP_AMBIG.!)

"x.`groupBy`(g).toMap.forall{ case (k,vs) => x.filter(xi => g(xi)==k) sameAs vs }".law

"x.`nonEmpty` implies x.`take`(1).nonEmpty".law

"x.`nonEmpty` implies { val h = x.`head`; x.`exists`(_ == h) }".law

"x.`nonEmpty` implies x.`take`(1).`forall`(_ == x.`head`)".law(SEQ)

"x.`headOption` == tryO{ x.`head` }".law(SEQ)

"""
x.`headOption` match {
  case None => x.`size` == 0
  case Some(y) => x.`exists`(_ == y)
}
""".law

"""
val k = x.`drop`(n).`takeWhile`(_ != a)
x.`indexOf`(a,n) match { 
  case -1 => k.`size` == (0 max x.size-n)
  case kk => n+k.size == kk && x.`drop`(kk).`head` == a 
}
""".law(SEQ, Filt.xsize(_ > 0))


"x.`indexOf`(a) == x.`indexOf`(a, 0)".law

"""
val i = x.`indexOfSlice`(y)
!x.`take`(math.max(0,i-1+y.size)).`containsSlice`(y) && ((i >= 0) implies (x.`drop`(i).`take`(y.size) sameAs y)) 
""".law(Filt.ysize(_ > 0))

"""
val i = x.`indexOfSlice`(y, n)
!x.`drop`(n).`take`(math.max(0,i-1-n+y.size)).`containsSlice`(y) && ((i >= 0) implies (i >= n && (x.`drop`(i).`take`(y.size) sameAs y)))
""".law(Filt.ysize(_ > 0))

"""
val k = x.`drop`(n).`takeWhile`(xi => !p(xi))
x.`indexWhere`(p,n) match { 
  case -1 => k.`size` == (0 max x.size-n)
  case kk => n+k.size == kk && p(x.`drop`(kk).`head`) 
}
""".law(Filt.xsize(_ > 0))

"x.`indexWhere`(p) == x.`indexWhere`(p, 0)".law

"""
val k = x.`takeWhile`(xi => !p(xi)).`size`
x.`indexWhere`(p) match { 
  case -1 => k == x.`size`
  case q => q == k
}
""".law

"x.`indices` == (0 until x.`size`)".law

"x.`size` > 0 implies (x.`init` sameAs x.`dropRight`(1))".law

"x.`size` > 0 implies (x.`init`.size == x.size - 1)".law

"x.`size` > 0 implies (x.`init` partOf x)".law

"""
x.`size` > 0 implies { 
  val xx = x.`inits`.toVector.map(_.toVector)
  (xx zip xx.`tail`).`forall`{ 
    case (a,b) => a.`size` - 1 == b.size && (b partOf a)
  } 
}
""".law(SEQ, Tags.SelectTag(ti => if (ti.inst.values.xsize <= 10) None else Some(Outcome.Skip.x)))

"""
val xx = x.`inits`.toVector.map(_.toVector)
(xx zip xx.`drop`(1)).`forall`{ case (a,b) => a.`init` sameAs b }
""".law(SEQ, Tags.SelectTag(ti => if (ti.inst.values.xsize <= 10) None else Some(Outcome.Skip.x)))

"x.`inits`.`size` == 1 + x.size".law(Tags.SelectTag(ti => if (ti.inst.values.xsize <= 10) None else Some(Outcome.Skip.x)))

"""
val xx = x.`inits`.toVector
(xx zip xx.drop(1)).forall{ case (a, b) => a.`size` == b.`size` + 1 }
""".law(Tags.SelectTag(ti => if (ti.inst.values.xsize <= 10) None else Some(Outcome.Skip.x)))

"""
val xx = x.`inits`.toVector
(xx zip xx.drop(1)).forall{ case (a, b) =>
  (b partOf a) && !(a partOf b)
}
""".law(Tags.SelectTag(ti => if (ti.inst.values.xsize <= 10) None else Some(Outcome.Skip.x)))

"x.`intersect`(y).`toSet` == (x.toSet & y.toSet)".law

"x sameType x.`intersect`(y)".law

"x.`iterator` sameAs x".law

"x.`size` > 0 implies (x.`takeRight`(1).`count`(_ == x.`last`) == 1)".law(SEQ)

"""
x.`lastIndexOf`(a,n) match {
  case -1 => x.`take`(n+1).`indexOf`(a) == -1
  case k => k == (n min x.`size`-1) - x.take(n+1).reverse.indexOf(a)
}
""".law


"x.`lastIndexOf`(a) == x.lastIndexOf(a,x.`size`-1)".law

"""
y.`size` > 0 implies { 
  val i = x.`lastIndexOfSlice`(y)
  !x.`drop`(math.max(0,i+1)).`containsSlice`(y) && 
  ((i >= 0) implies (x.`drop`(i).`take`(y.size) sameAs y))
}
""".law

"""
y.`size` > 0 implies { 
  val i = x.`lastIndexOfSlice`(y,r)
  !x.`take`(r).`drop`(math.max(0,i+1)).`containsSlice`(y) && 
  ((i >= 0) implies (i <= r && (x.`drop`(i).`take`(y.size) sameAs y)))
}
""".law

"""
x.`lastIndexWhere`(p,n) match { 
  case -1 => x.`take`(n+1).`indexWhere`(p) == -1
  case k => k == (n min x.`size`-1) - x.take(n+1).reverse.indexWhere(p) 
}
""".law

"x.`lastIndexWhere`(p) == x.lastIndexWhere(p,x.`size`-1)".law

"x.`lastOption` == tryO{ x.`last` }".law

"x.`lengthCompare`(n).signum == (x.`size` compare n).signum".law

"""
val xx = x.map(_.toString)
xx eq xx.`mapConserve`(identity)
""".law

"""
val size =
  x.toVector.groupBy(identity).
    map{ case (_,vs) => vs.size }.
    scanLeft((x.size,0)){ (c,i) => (c._1 - c._2, i) }.
    map{ case (n,k) => (1L to k).map(n+1 - _).product / (1L to k).product }.
    product
x.`permutations`.size == size
""".law(Filt.xsize(_ <= 8))  // Gets WAY too big above 8!

"x.`permutations`.size == x.`permutations`.toSet.size".law(Filt.xsize(_ <= 8))  // Too big above 8!

"""
val xs = x.toSet
x.`permutations`.forall(_.forall(xs))
""".law(Filt.xsize(_ <= 8))   // Too big above 8!

"""x.`prefixLength`(p) == x.`takeWhile`(p).`size`""".law

"""
val ix = x.`toIndexedSeq`
val k = x.`size`
var ki = 0
x.`reverse`.`forall`{ xi => ki += 1; xi == ix(k - ki) }
""".law(SEQ)

"x.`reverseIterator` sameAs x.`reverse`".law(SEQ)

"x.`reverseMap`(f) sameAs x.`reverse`.map(f)".law(SEQ)

"x.`reverseMap`(f) sameAs x.map(f).`reverse`".law(SEQ)

"x sameType x.`reverseMap`(f)".law(SPECTYPE.!)

"x.`reverse_:::`(y) sameAs x.`:::`(y.`reverse`)".law(SEQ)

"x sameType x.`reverse_:::`(y)".law

"x.`scan`(a)((l, r) => op(l, r)) sameAs x.toList.scanLeft(a)((l, r) => op(l, r))".law(SET.!, MAP.!)

"""
val ab = collection.mutable.ArrayBuffer(a)
x.foreach(xi => ab += (op(ab.last, xi)))
x.`scanLeft`(a)((acc, xi) => op(acc, xi)) sameAs ab
""".law(SET.!, MAP.!)

"x.`scanRight`(a)((xi, acc) => op(acc, xi)) sameAs x.toList.reverse.scanLeft(a)((acc, xi) => op(acc, xi)).reverse".law(SET.!, MAP.!)

"""
val temp = x.`scan`("")((s,xi) => s.toString + xi.toString).toList.sortBy(_.toString.length)
(temp zip temp.tail).forall{ case (a,b) => b.toString.startsWith(a.toString) }
""".law

"""
x.`scan`("")((s,xi) => s.toString + xi.toString).toList.
  sortBy(_.toString.length).lastOption.
  forall{ yN => yN.toString.toSeq.groupBy(identity) == x.map(_.toString).mkString.toSeq.groupBy(identity) }
""".law(BITSET_MAP_AMBIG.!)

"""
val temp = x.`scanLeft`(Set[A]())((s,xi) => s + xi).toList.sortBy(_.size)
(temp zip temp.tail).forall{ case (a,b) => a subsetOf b}
""".law

"x.`scanLeft`(Set[A]())((s,xi) => s + xi).toList.sortBy(_.size).lastOption.forall(_ sameAs x.toSet)".law(SET)

"""
val temp = x.`scanRight`(Set[A]())((xi,s) => s + xi).toList.sortBy(_.size)
(temp zip temp.tail).forall{ case (a,b) => a subsetOf b} && temp.lastOption.forall(_ sameAs x.toSet)
""".law(SET)

"x.`scanRight`(Set[A]())((xi,s) => s + xi).toList.sortBy(_.size).lastOption.forall(_ sameAs x.toSet)".law(SET)

"n <= 0 || x.`segmentLength`(p,n) == x.`drop`(n).`takeWhile`(p).`size`".law

"r <= 0 || x.`sliding`(r).`size` == (if (x.nonEmpty) math.max(0,x.`size`-r)+1 else 0)".law(QUEUE_SLIDE_11440.!)

"""
r <= 0 || 
x.`size` <= 0 || 
(r >= x.`size` && { x.`sliding`(r).map(_.toVector).toVector sameAs Vector(x.toVector) }) || 
{ val vx = x.toVector; x.`sliding`(r).toVector.map(_.toVector) sameAs Vector.range(0, 1+vx.size-r).map(i => vx.slice(i,i+r)) }
""".law(SEQ)

"""
var last: Option[A] = None
val i = x.`sorted`.`iterator`
var ordered = true
while (ordered && i.hasNext) {
  val a = i.next
  ordered = last.forall(_ <= a)
  last = Some(a)
}
ordered
""".law

"val x0 = x; x0.`sortInPlace`; x0 sameAs x.`sorted`".law

"x.`sortBy`(f) sameAs x.`sortWith`((a,b) => f(a) < f(b))".law(SORTWITH_INT_CCE.!)

"val x0 = x; x0.`sortInPlaceBy`(f); x0 sameAs x.`sortBy`(f)".law

"val x0 = x; x0.`sortInPlaceWith`((a, b) => f(a) < f(b)); x0 sameAs x.`sortWith`((a, b) => f(a) < f(b))".law

"""
val xx = x.`sortWith`((a,b) => f(a) > f(b)).toList
(xx zip xx.drop(1)).forall{ case (a,b) => !(f(a) < f(b)) }
""".law(SORTWITH_INT_CCE.!)

"x.`sorted` sameAs x.`toArray`.sorted".law // Need to add a custom ordering here

"""
val x0 = x.`sorted`
val lt_n = x0.`takeWhile`(_ < a).`size`
val lteq_n = x0.takeWhile(_ <= a).size
x0.`search`(a) match {
  case scala.collection.Searching.Found(i)          => i >= lt_n && i < lteq_n
  case scala.collection.Searching.InsertionPoint(i) => i == lt_n && i == lteq_n
}
""".law

"""
val (x1,x2) = x.`splitAt`(n)
(x1 sameAs x.`take`(n)) && (x2 sameAs x.`drop`(n))
""".law

"""
val (x1,x2) = x.`splitAt`(n)
(x1 sameType x2) && (x sameType x1)
""".law

"x.`startsWith`(y,n) implies (x.`drop`(n).`take`(y.`size`) sameAs y)".law(Filt.xsize(_ > 0))

"x.`startsWith`(y) == x.startsWith(y,0)".law

"xsize < 1 || { x.`tail` sameAs x.`drop`(1) }".law

"""
x.`tails`.toList.map(_.toList) match {
  case z => (z zip z.drop(1)).forall{
    case (a :: b :: _, c :: _) => b == c
    case (a, b) => b.isEmpty && (a.isEmpty || a.size == 1)
  }
}
""".law(SEQ, Tags.SelectTag(ti => if (ti.inst.values.xsize <= 10) None else Some(Outcome.Skip.x)))

"""
val xtl = x.`tails`.toList
(xtl zip xtl.drop(1)).forall{ case (a,b) => (b subsetOf a) && b.size == a.size-1 }
""".law(SET, Tags.SelectTag(ti => if (ti.inst.values.xsize <= 10) None else Some(Outcome.Skip.x)))

"""
val xtl = x.`tails`.toList
(xtl zip xtl.drop(1)).forall{ case (a,b) => (b.toSet subsetOf a.toSet) && b.size == a.size-1 }
""".law(SEQ.!, SET.!, Tags.SelectTag(ti => if (ti.inst.values.xsize <= 10) None else Some(Outcome.Skip.x)))

"x.`takeRight`(n) sameAs { val m = x.`size` - math.max(0, n); x.`drop`(m) }".law

"x sameType x.`takeRight`(n)".law

"""
x.map(a => collectionFrom(Array.fill(n)(a))).`transpose`.`forall`(_ sameAs x)
""".law(ORDERLY.!, BITSET_MAP_AMBIG.!, Filt.xsize(_ > 1), Filt.n(_ > 0))

"""
x.`unsorted`.map(a => collectionFrom(Array.fill(n)(a))).`transpose`.`forall`(_ sameAs x)
""".law(ORDERLY, Filt.xsize(_ > 1), Filt.n(_ > 0))

"x.`union`(y).`toSet` == (x.toSet union y.toSet)".law

"x sameType x.`union`(y)".law(ACC_SPEC.!)

"""
val xa = x.`zip`(y).`unzip`._1.`toArray`
x.`take`(xa.size) sameAs xa
""".law(BITSET_ZIP_AMBIG.!)

"""
val xb = x.`zip`(y).`unzip`._2.`toArray`
y.`take`(xb.size) sameAs xb
""".law(BITSET_ZIP_AMBIG.!)

"""
x.map(a => (a,a)).`unzip` match {
  case (y1, y2) => List(y1, y2).forall(_ sameAs x)
  case _ => false
}
""".law(BITSET_MAP_AMBIG.!)

"""
x.map(a => (a,a,a)).`unzip3` match { 
  case (y1, y2, y3) => List(y1, y2, y3).forall(_ sameAs x)
  case _ => false
}
""".law(BITSET_MAP_AMBIG.!)

"""
val v = Vector.newBuilder[A]
x.`withFilter`(p).foreach(v += _)
v.result sameAs x.`filter`(p).toVector
""".law(SEQ)

"""
val v = Vector.newBuilder[A]
x.`withFilter`(p).foreach(v += _)
v.result samePieces x.`filter`(p).toVector
""".law(SEQ.!)

"""
val z = x.`zip`(y).toArray
val k = math.min(x.`size`, y.size)
var i, j = -1
(
  x.`forall`{ xi => i += 1; i >= k || z(i)._1 == xi } &&
  y.`forall`{ yj => j += 1; j >= k || z(j)._2 == yj }
)
""".law(SET.!, MAP.!)

"""
val z = x.`zip`(y).toArray
(
  (z.map(_._1).toSet subsetOf x) &&
  (z.map(_._2).toSet subsetOf y) &&
  z.forall{ case (a,b) => (x contains a) && (y contains b) }
)
""".law(SET, BITSET_ZIP_AMBIG.!)

"""
val xSet = x.toSet
val ySet = y.toSet
val z = x.`zip`(y)
z.forall{ case (xi, yi) => xSet(xi) && ySet(yi) }
""".law(MAP)

"x sameAs x.`view`".law

"val x0 = x; x0.`++=`(y); x0 sameAs (x.`++`(y))".law

"val x0 = x; x0.`++=:`(y); x0 sameAs (y.`++`(x))".law

"val x0 = x; x0.`+=`(a); x0 sameAs (x.`:+`(a))".law

"val x0 = x; x0.`+=`(a); x0 sameAs (x.`+`(a))".law

"val x0 = x; x0.`+=:`(a); x0 sameAs (x.`+:`(a))".law

"(x.`-`(a)).`count`(_ == a) == (0 max x.count(_ == a) - 1)".law(MAP.!)

"(x.`-`(a._1)).`count`(_ == a) == 0".law(MAP)

"(x.`-`(a)).`size` == x.size - (if (x.`contains`(a)) 1 else 0)".law(MAP.!)

"(x.`-`(a._1)).`size` == x.size - (if (x.`contains`(a._1)) 1 else 0)".law(MAP)

"(x.`-`(a)) partOf x".law(MAP.!)

"(x.`-`(a._1)) partOf x".law(MAP)

"x sameType x.`-`(a)".law(MAP.!)

"x sameType x.`-`(a._1)".law(MAP)

"val x0 = x; x0.`-=`(a) sameAs x.`-`(a)".law(MAP.!)

"val x0 = x; x0.`-=`(a._1) sameAs x.`-`(a._1)".law(MAP)

"val x0 = x; x0.`--=`(y); val x1 = x; y.foreach(yi => x1.`-=`(yi)); x0 sameAs x1".law(MAP.!)

"val x0 = x; x0.`--=`(y.map(_._1)); val x1 = x; for ((k,_) <- y) { x1.`-=`(k) }; x0 sameAs x1".law(MAP)

"{ val z = x; z.`append`(a); z } == x.`+=`(a)".law

"{ val z = x; z.`appendAll`(y); z } == x.`++=`(y)".law

"{ val z = x; z.`prepend`(a); z } == x.`+=:`(a)".law

"{ val z = x; z.`prependAll`(y); z } == x.`++=:`(y)".law

"""
tryE{ val x0 = x;  x0.`remove`(n, nn); x0 } match { 
  case Left(_: IllegalArgumentException) => nn < 0
  case Left(_: IndexOutOfBoundsException) => n > (x.`size` - nn)
  case Right(x0) => x0 sameAs (x.`take`(n).`++`(x.`drop`(n+(0 max nn))))
  case _ => false
}
""".law(SET.!, MAP.!, Filt.xsize(_ > 0))

"""
val x0 = x
val a0 = x0.`remove`(n)
x0.`size` == x.`size`-1 && x.`apply`(n) == a0
""".law(SET.!, MAP.!, Filt.xsize(_ > 0))

"""
val x0, x1 = x
x0.`remove`(n)
x1.`remove`(n,1)
x0 sameAs x1
""".law(SET.!, MAP.!, Filt.xsize(_ > 0))

"x.`filterKeys`(k => p((k, a._2))) sameAs x.toVector.filter{ case (k, v) => p((k, a._2)) }.toMap".law

"x.`keySet` sameAs x.`iterator`.map{ case (k, v) => k }.toSet".law

"x.`keySet` sameAs x.`keys`".law

"x.`keysIterator`.toSet sameAs x.`keySet`".law

"x.`values` samePieces x.`iterator`.map{ case(k, v) => v }.toVector".law

"x.`valuesIterator`.toVector samePieces x.`iterator`.map{ case (k, v) => v }.toVector".law

"(!x.`contains`(a._1) && !y.`contains`(a._1)) implies x.`withDefaultValue`(a._2).apply(a._1) == a._2".law

/*
"x0.`remove`(a) == x0.`contains`(a)".law(SET)

"x0.`remove`(a._1) == x0.`contains`(a._1)".law(MAP)

"""
val x0 = x
val had = x0.`remove`(a)
if (had) (x0.`+`(a)) sameAs x else x0 sameAs x
""".law(SET)

"""
val x0 = x
val had = x0.`remove`(a._1)
if (had) x0.`size` + 1 == x.`size` && (x0 partOf x) else x0 sameAs x
""".law(MAP)
*/



"x.`result` sameAs x".law

"val x0 = x; x0.`transform`(f); x0 sameAs x.map(f)".law(MAP.!)

"""
val x0 = x
x0.`transform`((a, b) => f((a, b))._2)
x0 sameAs x.map{ case (a, b) => a -> f((a, b))._2 }
""".law(MAP)

"x.`updated`(n,a) sameAs (x.`take`(n).`:+`(a).`++`(x.`drop`(n+1)))".law(SEQ, Filt.xsize(_ > 0))

"x.`updated`(a._1, a._2).`forall`{ case (k,v) => if (k == a._1) v == a._2 else x.`get`(k).exists(_ == v) }".law(MAP)

"x sameType x.`updated`(n,a)".law(SEQ, SPECTYPE.!, Filt.xsize(_ > 0))

"x sameType x.`updated`(a._1, a._2)".law(MAP, CPMH_TYPE_11449.!, Filt.xsize(_ > 0))

"{ val x0 = x; x0.`update`(n, a); x0 sameAs x.`updated`(n,a) }".law(MAP.!, Filt.xsize(_ > 0))

"{ val x0 = x; x0.`update`(a._1, a._2); x0 sameAs x.`updated`(a._1, a._2) }".law(MAP)

"x.`&`(y) partOf x".law

"x.`forall`(xi => y.`contains`(xi) == x.`&`(y).contains(xi))".law

"x sameType x.`&`(y)".law

"x.`&~`(y) partOf x".law

"x.`forall`(xi => y.`contains`(xi) != x.`&~`(y).contains(xi))".law

"x sameType x.`&~`(y)".law

"x.`^`(y) == x.`xor`(y)".law

"val z = x.`|`(y); x.`forall`(z contains _) && y.`forall`(z contains _)".law

"val z = x.`|`(y); z.`forall`(i => x(i) || y(i))".law


///////////////////////////////////////
// New laws for strawman collections //
///////////////////////////////////////

"(x.`toVector` sameAs x.toVector.reverse) == (x.iterator sameAs x.toVector.reverse.iterator)".law(INSORD)

"(!x.`contains`(a._1) && !y.contains(a._1)) implies (x.`+`(a).`++`(y).iterator.toVector.`drop`(x.`size`).headOption == Some(a))".law(INSORD, MAP)

"x.`:++`(y) sameAs x.`++`(y)".law

"val x0 = x; val x1 = x; x0.`addOne`(a); x1.`+=`(a); x0 sameAs x1".law

"val x0 = x; val x1 = x; x0.`addAll`(y); x1.`++=`(y); x0 sameAs x1".law

"x.`appended`(a) sameAs x.`:+`(a)".law

"x.`appendedAll`(y) sameAs x.`:++`(y)".law

"val x0 = x; x0.`clear`; x0.`size` == 0".law

"x.`concat`(y) sameAs x.`++`(y)".law

"x.`distinctBy`(f).`size` == x.`map`(f).`toSet`.size".law

"""
val direct = x.`distinctBy`(f)
val built = Array.newBuilder[A]
val check = collection.mutable.HashSet.empty[A]
x.foreach{ xi =>
  val key = f(xi)
  if (!check(key)) {
    check += key
    built += xi
  }
}
direct samePieces built.result
""".law

"val x0 = x; x0.`dropInPlace`(n max 0); x0 sameAs x.`drop`(n)".law

"val x0 = x; x0.`dropRightInPlace`(n max 0); x0 sameAs x.`dropRight`(n)".law

"val x0 = x; x0.`dropWhileInPlace`(p); x0 sameAs x.`dropWhile`(p)".law

"val x0 = x; x0.`filterInPlace`(p); x0 sameAs x.`filter`(p)".law(MAP.!)

"val x0 = x; x0.`filterInPlace`((k, v) => p((k, v))); x0 sameAs x.`filter`(p)".law(MAP)

"val x0 = x; x0.`flatMapInPlace`(xi => y.toList.take(intFrom(xi))); x0 sameAs x.`flatMap`(xi => y.toList.take(intFrom(xi)))".law

"x.`groupMap`(f)(g).map(_._2.size).sum == x.`size`".law

"""
val m = collection.mutable.HashMap.empty[A, List[B]]
x.foreach{ xi => val key = f(xi); m(key) = g(xi) :: m.getOrElse(key, Nil) }
val gm = x.`groupMap`(f)(g)
m.size == gm.size &&
m.forall{ case (k, vs) => gm(k) sameAs vs.reverse } &&
gm.forall{ case (k, vs) => m(k).reverse sameAs vs }
""".law

"x.`groupMap`(g)(f).map{ case (k, vs) => k -> vs.`reduceLeft`(op) } samePieces x.`groupMapReduce`(g)(f)(op)".law(Filt.sym)

"val x0 = x; x0.`insert`(n max 0, a); x0 sameAs collectionFrom(x.toArray.patch(n max 0, Array(a), 0))".law(MAP.!, SET.!)

"val x0 = x; val x1 = x; x0.`insertAll`(n max 0, y); y.`reverse`.foreach(yi => x1.`insert`(n max 0, yi)); x0 sameAs x1".law(MAP.!)

"val x0 = x; x0.`mapInPlace`(f); x0 sameAs x.`map`(f)".law(PQ_MIP_NPE_11439.!)

"val x0 = x; x0.`mapValuesInPlace`((k: K, v: V) => f((k, v))._2); x0 sameAs x.`map`{ case (k, v) => k -> f((a._1, v))._2 }".law(MAP)

"x.`mapValues`(v => f((a._1, v))._2).toMap sameAs x.`map`{ case (k, v) => k -> f((a._1, v))._2 }".law(MAP)

"val x0 = x; x0.`padToInPlace`(n, a); x0 sameAs x.`padTo`(n, a)".law

"val x0 = x; x0.`patchInPlace`(n max 0, y, m); x0 sameAs x.`patch`(n max 0, y, m)".law(LISTBUF_PIP_11438.!)

"x.`prepended`(a) sameAs x.`+:`(a)".law

"x.`prependedAll`(y) sameAs x.`++:`(y)".law

"m > x.`size` || n < 0 || m < 0 || { val x0 = x; x0.`sliceInPlace`(n, m); x0 sameAs x.`slice`(n, m) }".law

"val x0 = x; x0.`subtractOne`(a); x0.`size` == xsize - (if (x.`contains`(a)) 1 else 0)".law(MAP.!)

"val x0 = x; x0.`subtractOne`(a); x0 sameAs (x.`takeWhile`(_ != a) ++ x.`dropWhile`(_ != a).drop(1))".law(MAP.!)

"val x0 = x; val x1 = x; x0.`subtractAll`(y); y.foreach(yi => x1.`subtractOne`(yi)); x0 sameAs x1".law(MAP.!)

"val x0 = x; x0.`takeInPlace`(n max 0); x0 sameAs x.`take`(n)".law

"val x0 = x; x0.`takeRightInPlace`(n max 0); x0 sameAs x.`takeRight`(n)".law

"val x0 = x; x0.`takeWhileInPlace`(p); x0 sameAs x.`takeWhile`(p)".law

"val x0 = x; val x1 = x; if (n >= 0) { x0.`trimStart`(n); x1.`dropInPlace`(n) }; x0 sameAs x1".law

"val x0 = x; val x1 = x; if (n >= 0) { x0.`trimEnd`(n); x1.`dropRightInPlace`(n) }; x0 sameAs x1".law

"x.`knownSize` match { case n if n >= 0 => n == x.`size`; case _ => true }".law

"""
var touched = 0
var ll = LazyList.from(0).map{ i => touched += 1; i }
(x.`lazyZip`(ll).take(n).map(_._1) samePieces x.`take`(n)) && (touched == (n max 0))
""".law

"""
val i = x.`iterator`
val s = x.`stepper`
var same = true
while (same && i.hasNext && s.hasStep) same = i.next == s.nextStep
!i.hasNext && !s.hasStep && same
""".law(STAGGER.!)

"""
val s = x.`stepper`
val k = x.`keyStepper`
var same = true
while (same && s.hasStep && k.hasStep) same = s.nextStep._1 == k.nextStep
!s.hasStep && !k.hasStep && same
""".law(STAGGER.!)

"""
val s = x.`stepper`
val v = x.`valueStepper`
var same = true
while (same && s.hasStep && v.hasStep) same = s.nextStep._2 == v.nextStep
!s.hasStep && !v.hasStep && same
""".law(STAGGER.!)


"""
val s = x.`stepper`
val built = Array.newBuilder[A]
while (s.hasStep) built += s.nextStep
built.result samePieces x
""".law

"val s = x.`stepper`; val e: collection.Stepper.EfficientSplit = s; e ne null".law(SPLITS, INEFFICIENT_BUG.!)

"""
val s = x.`stepper`
val ss = s.trySplit
val built = Array.newBuilder[A]
if (ss != null) while (ss.hasStep) built += ss.nextStep
while (s.hasStep) built += s.nextStep
built.result sameAs x
""".law(SEQ)

"""
val s = x.`stepper`
val ss = s.trySplit
val built = Array.newBuilder[A]
if (ss != null) while (ss.hasStep) built += ss.nextStep
while (s.hasStep) built += s.nextStep
built.result samePieces x
""".law(SEQ.!)

"""
val s = x.`stepper`
val ss = s.trySplit
val built = Array.newBuilder[A]
if (ss != null) while (ss.hasStep) built += ss.nextStep
while (s.hasStep) built += s.nextStep
built.result samePieces x
""".law(SEQ.!)

"""
val built = Array.newBuilder[A]
val xx = x.`tapEach`(built += _)
xx sameAs built.result
""".law(SEQ, ONCE.!, INDEF.!, VIEW.!)

"""
val built = Array.newBuilder[A]
val xx = x.`tapEach`(built += _)
xx samePieces built.result
""".law(SEQ.!, ONCE.!, INDEF.!, VIEW.!)

"x sameType x.`tapEach`(_ => ())".law

"x.`findLast`(p) == x.`reverse`.`find`(p)".law


/////////////////////////////
// End of individual laws. //
// Still inside Laws       //
// object.  Normal         //
// indentation resumes.    //
/////////////////////////////

  /** Complete listing of laws (including disabled ones that aren't used for code generation) */
  val complete = b.result

  /** Complete listing of laws that are not disabled */
  val all = complete.filterNot(_.disabled)

  /** Map from line numbers to the corresponding law. */
  lazy val byLineNumber = complete.map(law => law.lineNumber -> law).toMap
}
