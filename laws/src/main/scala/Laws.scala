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
  private[this] def findMyMethods: Either[FormatErr, Set[String]] = {
    val b = Array.newBuilder[String]
    var i = 0
    while (i >= 0 && i < code.length) {
      i = code.indexOf('`', i)
      if (i >= 0) {
        val j = code.indexOf('`', i+1)
        if (j > i+1) b += code.substring(i+1, j)
        else return Left(FormatErr("Unclosed method quotes", code, i, code.substring(i)))
        i = j+1
      }
    }
    Right(b.result.toSet)
  }

  /** Declare that a tag should not be present */
  def not(t: Tag): Law = new Law(name, tags shun t, code, disabled)(file, line)

  /** Declare that several tags should not be present */
  def not(t1: Tag, t2: Tag, tx: Tag*): Law = new Law(name, ((tags shun t1 shun t2) /: tx)((ts, t) => ts shun t), code, disabled)(file, line)

  /** Declare that an additional tag must be present */
  def and(t: Tag): Law = new Law(name, tags need t, code, disabled)(file, line)

  /** Declare that several tags must be present */
  def add(t1: Tag, t2: Tag, tx: Tag*): Law = new Law(name, ((tags need t1 need t2) /: tx)((ts, t) => ts need t), code, disabled)(file, line)

  /** Add another check of TestInfo for some property */
  def filter(p: TestInfo => Option[Outcome.Skip]): Law = new Law(name, tags.filter(p), code, disabled)(file, line)

  /** Methods in the law that are backtick-quoted, indicating that the collection should only be used if it has those methods */
  val methods = findMyMethods

  /** Function that checks whether a set of methods contains the methods needed to run this law */
  val checker = findMyMethods.fold(_ => MethodChecker.empty, s => new MethodChecker(s))

  val cleanCode = findMyMethods.
    map(ms => (code /: ms)((c, m) => c.replace(f"`$m`": CharSequence, m: CharSequence))).
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
  def apply(code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = new Law(code)(file, line)
  def apply(name: String, code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = new Law(name, code)(file, line)
  def apply(tags: Tags, code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = new Law(tags, code)(file, line)  
}


object Laws {
  import Tag._
  import Tags.Implicits._

  private val b = Array.newBuilder[Law]

  /** Implicits used to make the definition of laws as low-boilerplate as possible */
  implicit class LawMaker(rawText: String)(implicit file: sourcecode.File, line: sourcecode.Line) {
    /** Used to force implicit resolution of this class if it's otherwise ambiguous */
    def make: this.type = this

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
    def skip {
      b += new Law("", Tags.empty, text, disabled = true)
    }

    /** A law with no tags */
    def law { 
      b += new Law(text)
    }

    /** A law with tags */
    def law(tag: Tags.Taggish, more: Tags.Taggish*) {
      b += new Law(Tags(tag, more: _*), text)
    }

    /** A named law with no tags */
    def named(name: String) {
      b += new Law(name, text)
    }

    /** A named law with tags */
    def named(name: String, tag: Tags.Taggish, more: Tags.Taggish*) {
      b += new Law(name, Tags(tag, more: _*), text)
    }

    /** A law with no tags but a setter to place what you need. */
    def lawFn(f: Law => Law) {
      b += f(new Law(text))
    }

    /** A law that's filtered based on TestInfo */
    def lawFilt(p: TestInfo => Option[Outcome.Skip]) {
      lawFn(_.filter(p))
    }

    /** A law that's filtered based on TestInfo */
    def lawFilt(p: TestInfo => Boolean, blame: Outcome.Skip) {
      lawFn(_.filter(z => if (p(z)) None else Some(blame)))
    }
  }

///////////////////////////
// Individual laws begin //
///////////////////////////

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

"x.map(f) theSameAs { val y = collection.mutable.ArrayBuffer.empty[A]; x.foreach(xi => y += f(xi)); y }".law(SEQ)

"x.map(f) theSameAs { val y = collection.mutable.HashSet.empty[A]; x.foreach(y += _); y.map(f) }".law(SET)

// x sameType x.map(f)

"""{
  val flat = x.flatMap(xi => y.toList.take(intFrom(xi)))
  val ref = collection.mutable.ArrayBuffer.empty[A]
  x.foreach(xi => ref ++= y.toList.take(intFrom(xi)))
  flat theSameAs ref
}""".law(SEQ)

"""{
  val flat = x.flatMap(xi => y.toList.take(intFrom(xi)))
  val ref = collection.mutable.HashSet.empty[A]
  x.foreach(xi => ref ++= y.toList.take(intFrom(xi)))
  flat theSameAs ref
}""".law(SET)

// x sameType x.flatMap(xi => y.toList.take($INT(xi))))

"x.`exists`(p) == x.`find`(p).isDefined".law

"x.`forall`(p) implies (x.`isEmpty` || x.`exists`(p))".law

"{ var y = false; x.`foreach`(xi => y |= p(xi)); y == x.`exists`(p) }".law

"x.`repr` theSameAs x".law

"x.`toIterator` theSameAs x".law

"x.`toStream` theSameAs x".law

"x.`toTraversable` theSameAs x".law

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

"""
val xx = x.`genericBuilder`[A]
x.foreach{ xx += _ }
x theSameAs xx.result
""".law

// !RANGE !SORTED !ARRAY !USESARRAY !N ... { val xx = x.`genericBuilder`[@A]; sameType(x, xx.result) }

/******** Everything else (roughly alphabetical) ********/

"x.`aggregate`(zero)((b,a) => b, (b1,b2) => b1) == zero".lawFilt(_.skipMissingZero)

"x.`aggregate`(zero)((b,a) => b, (b1,b2) => b2) == zero".lawFilt(_.skipMissingZero)

"""
val arr = new Array[A](m)
x.`copyToArray`(arr, n, m)
(arr.drop(n).take(m) zip x.toList).forall{ case (x,y) => x==y }
""".law(SET.!, MAP.!)


"""
val arr = new Array[A](m)
x.`copyToArray`(arr, n, m)
val c = arr.drop(n).take(m min x.`size`)
val cs = c.toSet
c.size == cs.size && (cs subsetOf x.toSet)
""".law(SET)

"""
val arr = new Array[A](m)
val x0 = x
x0.`copyToArray`(arr, n, m)
(n until ((n+m) min x.`size`)).forall(i => x0.get(arr(i)._1).exists(_ == arr(i)._2))
""".law(MAP)


"x.`collectFirst`(pf).isDefined == x.`exists`(pf.isDefinedAt)".law

"x.`collectFirst`(pf) == x.`dropWhile`(xi => !pf.isDefinedAt(xi)).`headOption`.map(pf)".law

"""
val b = new collection.mutable.ArrayBuffer[A]
x.`copyToBuffer`(b)
b.result theSameAs x
""".law

"x theSameAs x.`companion`(x.toList: _*)".law

// !RANGE !SORTED !ARRAY !USESARRAY !N ... sameType(x, x.`companion`.empty[@A])

"x.`count`(p) > 0 == x.`exists`(p)".law

"(x.`count`(p) == x.`size`) == x.`forall`(p)".law

"x.`count`(p) == { var k=0; x.`foreach`(xi => if (p(xi)) k += 1); k }".law

"x.filter(p).`size` == x.`count`(p)".law

"x.filter(p).`forall`(p) == true".law

// p !RANGE ... sameType(x, x.filter(p))

"x.flatMap(xi => y.toList.take(intFrom(xi))) theSameAs x.map(xi => y.toList.take(intFrom(xi))).`flatten`".law(MAP.!)

"x.`foldLeft`(a)(op) == x.`foldRight`(a)(op)".lawFn(_.filter(_.skipAsymmetric))

"x.`foldLeft`(a)(op) == x.`/:`(a)(op)".law

"""x.`foldRight`(a)(op) == x.`:\`(a)(op)""".law

// !ADEFINITE ... x.`nonEmpty` implies (x.`hasDefiniteSize` == @DEFINITE)
// x.`isTraversableAgain` == @AGAIN

"tryO{x.`max`} == tryO{ x.`reduce`(maxOf) }".law

"tryO{x.`maxBy`(f)} == tryO{ val fx = x.map(f).`max`; x.find(xi => f(xi)==fx).get }".law

"tryO{x.`min`} == tryO{ x.`reduce`(minOf) }".law

"tryO{x.`minBy`(f)} == tryO{ val fx = x.map(f).`min`; x.find(xi => f(xi)==fx).get }".law

"x.`nonEmpty` == x.`exists`(_ => true)".law

"x.`product` == x.`fold`(1)(_ * _)".law(INT)

"x.`sum` == x.`fold`(0)(_ + _)".law(INT)

"""
Set(
  tryO{x.`reduce`(op)}, tryO{x.`reduceLeft`(op)}, tryO{x.`reduceRight`(op)},
  x.`reduceOption`(op), x.`reduceLeftOption`(op), x.`reduceRightOption`(op)
).size == 1
""".lawFn(_.filter(_.skipNonassociative))

"x.`size` == x.`count`(_ => true)".law

"x theSameAs x.`to`[List]".law

"x.`toArray` theSameAs x".law

"x.`toBuffer` theSameAs x".law

"x.`toIndexedSeq` theSameAs x".law

"x.`toIterable` theSameAs x".law

"x.`toList` theSameAs x".law

"x.map(xi => (xi,xi)).`toMap` correspondsTo { val hm = new collection.mutable.HashMap[A,A]; x.foreach(xi => hm += xi -> xi); hm }".law

"x.`toSeq` theSameAs x".law

"x.`toSet` isPartOf x".law

"x.`toVector` theSameAs x".law

"""
val c = new collection.mutable.ArrayBuffer[A];
x.`withFilter`(p).foreach(c += _);
c theSameAs x.filter(p)""".law(SET.!, MAP.!)

"""
val c = new collection.mutable.HashSet[A];
x.`withFilter`(p).foreach(c += _);
c theSameAs x.filter(p)""".law(SET)

"x.`hasNext` == x.`nonEmpty`".law

"x.`hasNext` implies tryO{ x.`next` }.isDefined".law

"x.`++`(y).`size` == x.size + y.size".law(SET.!, MAP.!)

"x.`++`(y).`size` <= x.size + y.size".law(SET, MAP)

"x.`++`(y).`size` == x.size + y.filter(yi => !x.`contains`(yi)).size".law(SET)

"x.`++`(y).`size` == x.size + y.filter(yi => !x.`contains`(yi._1)).size".law(MAP)

"(x.`++`(y)).`take`(x.`size`) theSameAs x".law(SEQ)

"(x.`++`(y)).`drop`(x.`size`) theSameAs y".law(SEQ)

"x.`--`(y).`size` >= x.size - y.size".law(MAP.!)


"x.`--`(y.map(_._1)).`size` >= x.size - y.size".law(MAP)

"(x.`--`(y)) isPartOf x".law(MAP.!)

"x.`--`(y.map(_._1)) isPartOf x".law(MAP)

"""
val List(q, qq, qqq) = List(x, y, x.`--`(y)).map(_.groupBy(identity).mapValues(_.size));
q.forall{ case (k,v) => v == qq.getOrElse(k,0) + qqq.getOrElse(k,0) }""".law(MAP.!)
// Need version that works for maps

// y !RANGE !MUVU !SI8814 ... sameType(x, x.`++`(y))

"x.`buffered` theSameAs x".law

"x.`collect`(pf) theSameAs x.`filter`(pf.isDefinedAt).`map`(pf)".law

// pf !RANGE !MUVU !SI6462 ... sameType(x, x.`collect`(pf))

"x.`contains`(a) == x.`exists`(_ == a)".law(MAP.!)

"x.`contains`(a._1) == x.`exists`(_._1 == a._1)".law(MAP)

"x.`corresponds`(x)(_ == _)".law

"(x.`size` != y.size) implies !x.`corresponds`(y)(_ == _)".law

"x.`corresponds`(y)((_,_) => false) implies !x.`nonEmpty` && !y.`nonEmpty`".law

"x.`drop`(n).`size` == (0 max (x.size-(0 max n)))".law

"x.`drop`(n) isPartOf x".law

// n ... sameType(x, x.`drop`(n))

"""
val c = x.`dropWhile`(p);
c.`take`(1).toList match { 
  case Nil => true
  case List(xi) => !p(xi)
  case _ => false
}""".law(SET.!)

"""
val y = x.`dropWhile`(p)
y.nonEmpty implies y.exists(yi => !p(yi))
""".law

"""x.`dropWhile`(p) isPartOf x""".law

//p ... sameType(x, x.`dropWhile`(p))

"""
val (x1,x2) = x.`duplicate`
x1.`corresponds`(x)(_ == _) && x2.corresponds(x)(_ == _)
""".law

// p ... val (x1,x2) = x.`duplicate`; sameType(x1, x2)

"x.`filterNot`(p) theSameAs x.`filter`(xi => !p(xi))".law

// p !RANGE ... sameType(x, x.`filterNot`(p))

"n <= 0 || x.`grouped`(n).map(_.size).`sum` == x.`size`".law

"""n <= 0 || m < 0 || {
  var y = 0; x.`grouped`(n).`drop`(m).take(1).map(_.`size`).foreach(y = _); (y < n) implies !x.grouped(n).drop(m+1).`nonEmpty`
}""".law

"x.`grouped`((1 max n)).flatMap(xi => xi) theSameAs x".law

"x.`isEmpty` == !x.`nonEmpty`".law

"x.`length` == x.`size`".law

"x.`padTo`(n, a).`size` == (n max x.size)".law

"x.`padTo`(n, a).`drop`(x.`size`).`forall`(_ == a)".law

"(n <= x.`size`) implies (x.`padTo`(n, a) theSameAs x)".law

// a n !RANGE !MUVU ... sameType(x, x.`padTo`(n,a))

"""
val (t,f) = x.`partition`(p)
(t theSameAs x.`filter`(p)) && (f theSameAs x.`filterNot`(p))
""".law

// p !RANGE ... val (t,f) = x.`partition`(p); sameType(t,f) && sameType(x,t)

"(n <= x.`size`) implies (x.`patch`(n, y, m).`take`(n) theSameAs x.take(n))".law

"(n <= x.`size`) implies (x.`patch`(n, y, m).`drop`(n).`take`(y.`size`) theSameAs y)".law

"(n <= x.`size`) implies (x.`patch`(n, y, m).`drop`((0 max n)+y.`size`) theSameAs x.`drop`((0 max n)+(0 max m)))".law

"(x `sameElements` y) == (x theSameAs y)".law

"n < 0 || m >= x.size || { x.`slice`(n, m).`size` == (0 max ((0 max m)-n)) }".law

"n < 0 || m >= x.size || { x.`slice`(n, m) theSameAs x.`drop`(n).`take`((0 max m)-n) }".law(SET.!)

//n m !SI8819 ... n < 0 || m >= x.size || { sameType(x, x.`slice`(n, m)) }

"x.`span`(p)._1.`forall`(p)".law

"(x.span(p)._2.`size`) > 0 implies !x.`span`(p)._2.`take`(1).`exists`(p)".law

"(x.span(p)._2.`size`) > 0 implies !x.`span`(p)._2.`forall`(p)".law

"val (x1, x2) = x.`span`(p); val n = x.`span`(p)._1.`size`; (x1 theSameAs x.`take`(n)) && (x2 theSameAs x.`drop`(n))".law

//p ... val (x1, x2) = x.`span`(p); sameType(x1, x2) && sameType(x, x1)

"x.`take`(n).`size` == ((0 max n) min x.size)".law

"x.`take`(n) isPartOf x".law

//n ... sameType(x, x.`take`(n))

"x.`takeWhile`(p).`forall`(p)".law

"x.`takeWhile`(p) isPartOf x".law

"x.`takeWhile`(p).`size` + x.`dropWhile`(p).size == x.size".law

//p ... sameType(x, x.`takeWhile`(p))

"x.`zip`(y).map(_._1) theSameAs x.take(x.size min y.size)".law

"x.`zip`(y).map(_._2) theSameAs y.take(x.size min y.size)".law

//y !N !MUVU !SI6462 ... sameType(x, x.`zip`(y).map(_._1))

"x.`zipAll`(y, a, b).map(_._1) theSameAs x.`padTo`(x.`size` max y.size, a)".law

"x.`zipAll`(y, a, b).map(_._2) theSameAs y.`padTo`(x.`size` max y.size, b)".law

//y a b !N !MUVU !SI6462 ... sameType(x, x.`zipAll`(y, a, b).map(_._1))

"x.`zipWithIndex`.map(_._1) theSameAs x".law

"x.`zipWithIndex`.map(_._2) theSameAs (0 until x.`size`)".law


//!N !MUVU !SI6462 ... sameType(x, x.`zipWithIndex`.map(_._1))

"x.`:++`(y) theSameAs x.`++`(y)".law

"x.`++:`(y) theSameAs y.`++`(x)".law

"x.`::`(a).`size` == x.size+1".law

"x.`::`(a).`head` == a".law

//a ... sameType(x, x.`::`(a))

"x.`+:`(a).`size` == x.size+1".law

"x.`+:`(a).`head` == a".law

//a !RANGE !MUVU ... sameType(x, x.`+:`(a))

"x.`:+`(a).`size` == x.size+1".law

"x.`:+`(a).`last` == a".law

//a !RANGE !MUVU ... sameType(x, x.`:+`(a))

"val s = x.`+`(a).`size` - x.size; 0 <= s && s <= 1".law

"x.`+`(a).`contains`(a)".law(MAP.!)

"x.`+`(a).`contains`(a._1)".law(MAP)

//a !RANGE !MUVU !SI8814 ... sameType(x, x.`+`(a))

"x.`:::`(y) theSameAs y.`++`(x)".law

//y ... sameType(x, x.`:::`(y))

"(n < x.`size`) implies (x.`apply`(n) == x.`drop`(n).`head`)".law(SEQ)

"""x.`size` > 8 || { 
  val n = x.`combinations`(r).size;
  ((r > x.size) implies (n == 0)) && x.combinations(r).toSet.size == n
}""".law

"""x.`size` > 8 || r == 0 || r > x.size || { 
  val s = x.toVector
  x.`combinations`(r).forall(_ isPartOf s)
}""".law

"""
x.`containsSlice`(y) implies (
  y.`size` == 0 ||
  ( Iterator.from(0).
    map(x.`drop`).
    takeWhile(_.`nonEmpty`).
    exists(_.`take`(y.`size`) theSameAs y)
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

"x.`dropRight`(nn) theSameAs x.`take`(x.`size` - math.max(0, nn))".law(SET.!)

//sameType(x, x.`reverse`)

"x.`endsWith`(y) == (x.`drop`(math.max(0, x.`size`-y.size)) theSameAs y)".law

"x.`groupBy`(g).keySet correspondsTo x.map(g).toSet".law

"x.`groupBy`(g).toMap.forall{ case (k,vs) => x.filter(xi => g(xi)==k) theSameAs vs }".law



"x.`nonEmpty` implies x.`take`(1).nonEmpty".law

"x.`nonEmpty` implies x.`take`(1).`forall`(_ == x.`head`)".law(SEQ)

"x.`headOption` == tryO{ x.`head` }".law(SEQ)

"""
val k = x.`drop`(n).`takeWhile`(_ != a)
x.`indexOf`(a,n) match { 
  case -1 => k.`size` == (0 max x.size-n)
  case kk => n+k.size == kk && x.`drop`(kk).`head` == a 
}
""".law(SEQ)


"x.`indexOf`(a) == x.`indexOf`(a, 0)".law

"""
y.`size` == 0 || { 
  val i = x.`indexOfSlice`(y)
  !x.`take`(math.max(0,i-1+y.size)).`containsSlice`(y) && ((i >= 0) implies (x.`drop`(i).`take`(y.size) theSameAs y)) 
}
""".law

"""
y.`size` == 0 || { 
  val i = x.`indexOfSlice`(y,r)
  !x.`drop`(r).`take`(math.max(0,i-1-r+y.size)).`containsSlice`(y) && ((i >= 0) implies (i >= r && (x.`drop`(i).`take`(y.size) theSameAs y)))
}
""".law

"""
val k = x.`drop`(n).`takeWhile`(xi => !p(xi))
x.`indexWhere`(p,n) match { 
  case -1 => k.`size` == (0 max x.size-n)
  case kk => n+k.size == kk && p(x.`drop`(kk).`head`) 
}
""".law

"x.`indexWhere`(p) == x.`indexWhere`(p, 0)".law

"""
val k = x.`takeWhile`(xi => !p(xi)).`size`
x.`indexWhere`(p) match { 
  case -1 => k == x.`size`
  case q => q == k
}
""".law

"x.`indices` == (0 until x.`size`)".law

"x.`size` > 0 implies (x.`init` theSameAs x.`dropRight`(1))".law

"x.`size` > 0 implies (x.`init`.size == x.size - 1)".law

"x.`size` > 0 implies (x.`init` isPartOf x)".law

"""
x.`size` > 0 implies { 
  val xx = x.`inits`.toVector.map(_.toVector)
  (xx zip xx.`tail`).`forall`{ 
    case (a,b) => a.`size` - 1 == b.size && (b isPartOf a)
  } 
}
""".law(SEQ)

"""
val xx = x.`inits`.toVector.map(_.toVector)
(xx zip xx.`drop`(1)).`forall`{ case (a,b) => a.`init` theSameAs b }
""".law(SEQ)

"x.`intersect`(y).`toSet` == (x.toSet & y.toSet)".law

// y !RANGE ... sameType(x, x.`intersect`(y))

"x.`iterator` theSameAs x".law

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
  ((i >= 0) implies (x.`drop`(i).`take`(y.size) theSameAs y))
}
""".law

"""
y.`size` > 0 implies { 
  val i = x.`lastIndexOfSlice`(y,r)
  !x.`take`(r).`drop`(math.max(0,i+1)).`containsSlice`(y) && 
  ((i >= 0) implies (i <= r && (x.`drop`(i).`take`(y.size) theSameAs y)))
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
""".lawFilt(_.inst.values.xsize <= 8, Outcome.Skip.x)  // Gets WAY too big above 8!

"""x.`permutations`.size == x.`permutations`.toSet.size""".lawFilt(_.inst.values.xsize <= 8, Outcome.Skip.x)  // Too big above 8!

"""
val xs = x.toSet
x.`permutations`.forall(_.forall(xs))
""".lawFilt(_.inst.values.xsize <= 8, Outcome.Skip.x)   // Too big above 8!

"""x.`prefixLength`(p) == x.`takeWhile`(p).`size`""".law

"""
val ix = x.`toIndexedSeq`
val k = x.`size`
var ki = 0
x.`reverse`.`forall`{ xi => ki += 1; xi == ix(k - ki) }
""".law

"x.`reverseIterator` theSameAs x.`reverse`".law

"x.`reverseMap`(f) theSameAs x.`reverse`.map(f)".law

"x.`reverseMap`(f) theSameAs x.map(f).`reverse`".law

// f !RANGE !MUVU ... sameType(x, x.`reverseMap`(f))

"x.`reverse_:::`(y) theSameAs x.`:::`(y.`reverse`)".law

// y ... sameType(x, x.`reverse_:::`(y))

"x.`scan`(a)((l, r) => op(l, r)) theSameAs x.toList.scanLeft(a)((l, r) => op(l, r))".law(SET.!, MAP.!)

"""
val ab = collection.mutable.ArrayBuffer(a)
x.foreach(xi => ab += (op(ab.last, xi)))
x.`scanLeft`(a)((acc, xi) => op(acc, xi)) theSameAs ab
""".law(SET.!, MAP.!)

"x.`scanRight`(a)((xi, acc) => op(xi, acc)) theSameAs x.toList.reverse.scanLeft(a)((acc, xi) => op(acc, xi)).reverse".law(SET.!, MAP.!)

"""
val temp = x.`scan`("")((s,xi) => s.toString + xi.toString).toList.sortBy(_.toString.length)
(temp zip temp.tail).forall{ case (a,b) => b.toString.startsWith(a.toString) }
""".law

"""
x.`scan`("")((s,xi) => s.toString + xi.toString).toList.
  sortBy(_.toString.length).lastOption.
  forall{ yN => yN.toString.groupBy(identity) == x.map(_.toString).mkString.groupBy(identity) }
""".law

"""
val temp = x.`scanLeft`(Set[A]())((s,xi) => s + xi).toList.sortBy(_.size)
(temp zip temp.tail).forall{ case (a,b) => a subsetOf b}
""".law

"x.`scanLeft`(Set[A]())((s,xi) => s + xi).toList.sortBy(_.size).lastOption.forall(_ == x.toSet)".law(SET)

"""
val temp = x.`scanRight`(Set[A]())((xi,s) => s + xi).toList.sortBy(_.size)
(temp zip temp.tail).forall{ case (a,b) => a subsetOf b} && temp.lastOption.forall(_ == x.toSet)
""".law(SET)

"x.`scanRight`(Set[A]())((xi,s) => s + xi).toList.sortBy(_.size).lastOption.forall(_ == x.toSet)".law(SET)

"n <= 0 || x.`segmentLength`(p,n) == x.`drop`(n).`takeWhile`(p).`size`".law

"r <= 0 || x.`sliding`(r).`size` == (if (x.nonEmpty) math.max(0,x.`size`-r)+1 else 0)".law

"""
r <= 0 || 
x.`size` <= 0 || 
(r >= x.`size` && { x.`sliding`(r).map(_.toVector).toVector == Vector(x.toVector) }) || 
{ val vx = x.toVector; x.`sliding`(r).toVector.map(_.toVector) == Vector.range(0, 1+vx.size-r).map(i => vx.slice(i,i+r)) }
""".law(SET.!)

"x.`sortBy`(f) theSameAs x.`sortWith`((a,b) => f(a) < f(b))".law

"""
val xx = x.`sortWith`((a,b) => f(a) > f(b)).toList
(xx zip xx.drop(1)).forall{ case (a,b) => !(f(a) < f(b)) }
""".law

"x.`sorted` theSameAs x.`toArray`.sorted".law // Need to add a custom ordering here

"""
val (x1,x2) = x.`splitAt`(n)
(x1 theSameAs x.`take`(n)) && (x2 theSameAs x.`drop`(n))
""".law

// val (x1,x2) = x.`splitAt`(n); sameType(x1, x2) && sameType(x, x1)

"x.`startsWith`(y,n) implies (x.`drop`(n).`take`(y.`size`) theSameAs y)".law

"x.`startsWith`(y) == x.startsWith(y,0)".law

"xsize < 1 || { x.`tail` theSameAs x.`drop`(1) }".law

"""
x.`tails`.toList.map(_.toList) match {
  case z => (z zip z.drop(1)).forall{
    case (a :: b :: _, c :: _) => b == c
    case (a, b) => b.isEmpty && (a.isEmpty || a.size == 1)
  }
}
""".law(SET.!, MAP.!)

"""
val xtl = x.`tails`.toList
(xtl zip xtl.drop(1)).forall{ case (a,b) => (b subsetOf a) && b.size == a.size-1 }
""".law(SET)

"""
val xtl = x.`tails`.toList
(xtl zip xtl.drop(1)).forall{ case (a,b) => (b.toSet subsetOf a.toSet) && b.size == a.size-1 }
""".law(MAP)

"x.`takeRight`(n) theSameAs { val m = x.`size` - math.max(0, n); x.`drop`(m) }".law

// n !RANGE ... sameType(x, x.`takeRight`(n))

"x.map(a => List.fill(n)(a)).`transpose`.`forall`(_ theSameAs x)".lawFilt(_.num.n > 0, Outcome.Skip.n)

// n !RANGE ARRAY ... n < 1 || x.map(a => Array.fill(n)(a)).`transpose`.`forall`(_ theSameAs x)

"x.`union`(y).`toSet` == (x.toSet union y.toSet)".law

// y !RANGE !MUVU ... sameType(x, x.`union`(y))

"""
val xa = x.`zip`(y).`unzip`._1.`toArray`
x.`take`(xa.size) theSameAs xa
""".law

"""
val xb = x.`zip`(y).`unzip`._2.`toArray`
y.`take`(xb.size) theSameAs xb
""".law

"""
x.map(a => (a,a)).`unzip` match {
  case (y1, y2) => List(y1, y2).forall(_ theSameAs x)
    case _ => false
}
""".law

"""
x.map(a => (a,a,a)).`unzip3` match { 
  case (y1, y2, y3) => List(y1, y2, y3).forall(_ theSameAs x)
  case _ => false
}
""".law

"""
val z = x.`zip`(y).toArray
val k = math.min(x.`size`, y.size)
var i, j = -1
(
  x.`forall`{ xi => i += 1; i >= k || z(i)._1 == xi } &&
  y.forall{ yj => j += 1; j >= k || z(j)._2 == yj }
)
""".law(SET.!, MAP.!)

"""
val z = x.`zip`(y).toArray
(
  (z.map(_._1).toSet subsetOf x) &&
  (z.map(_._2).toSet subsetOf y) &&
  z.forall{ case (a,b) => (x contains a) && (y contains b) }
)
""".law(SET)

"""
val z = x.`zip`(y)
val z2 = x.toSet.flatMap(z.get)
x.forall(z contains _) && y.forall(z2 contains _)
""".law(MAP)

"x theSameAs x.`view`".law

"val x0 = x; x0.`++=`(y); x0 theSameAs (x.`++`(y))".law

"val x0 = x; x0.`++=:`(y); x0 theSameAs (y.`++`(x))".law

"val x0 = x; x0.`+=`(a); x0 theSameAs (x.`:+`(a))".law

"val x0 = x; x0.`+=`(a); x0 theSameAs (x.`+`(a))".law

"val x0 = x; x0.`+=:`(a); x0 theSameAs (x.`+:`(a))".law

"(x.`-`(a)).`count`(_ == a) == (0 max x.count(_ == a) - 1)".law(MAP.!)

"(x.`-`(a._1)).`count`(_ == a) == 0".law(MAP)

"(x.`-`(a)).`size` == x.size - (if (x.`contains`(a)) 1 else 0)".law(MAP.!)

"(x.`-`(a._1)).`size` == x.size - (if (x.`contains`(a._1)) 1 else 0)".law(MAP)

"(x.`-`(a)) isPartOf x".law(MAP.!)

"(x.`-`(a._1)) isPartOf x".law(MAP)

// a !RANGE !M ... sameType(x, x.`-`(a))
// a M ... sameType(x, x.`-`(a._1))

"val x0 = x; x0.`-=`(a) theSameAs x.`-`(a)".law(MAP.!)

"val x0 = x; x0.`-=`(a._1) theSameAs x.`-`(a._1)".law(MAP)

"val x0 = x; x0.`--=`(y); val x1 = x; y.foreach(yi => x1.`-=`(yi)); x0 theSameAs x1".law(MAP.!)

"val x0 = x; x0.`--=`(y.map(_._1)); val x1 = x; for ((k,_) <- y) { x1.`-=`(k) }; x0 theSameAs x1".law(MAP)

"{ val z = x; z.`append`(a); z } == x.`+=`(a)".law

"{ val z = x; z.`appendAll`(y); z } == x.`++=`(y)".law

"{ val z = x; z.`prepend`(a); z } == x.`+=:`(a)".law

"{ val z = x; z.`prependAll`(y); z } == x.`++=:`(y)".law

"val x0 = x; x0.`reduceToSize`(n); x0.`size` == n".law

"val x0 = x; x0.`reduceToSize`(n); x0 theSameAs x.`take`(n)".law(SET.!)

"""
tryE{ val x0 = x;  x0.`remove`(n, mm); x0 } match { 
  case Left(_: IllegalArgumentException) => m < 0
  case Left(_: IndexOutOfBoundsException) => n > (x.`size` - mm)
  case Right(x0) => x0 theSameAs (x.`take`(n).`++`(x.`drop`(n+(0 max m))))
  case _ => false
}
""".law(SET.!, MAP.!)

"""
val x0 = x
val a0 = x0.`remove`(n)
x0.`size` == x.`size`-1 && x.`apply`(n) == a0
""".law(SET.!, MAP.!)

"""
val x0, x1 = x
x0.`remove`(n)
x1.`remove`(n,1)
x0 theSameAs x1
""".law(SET.!, MAP.!)

"x.`result` theSameAs x".law

"val x0 = x; x0.`transform`(f); x0 theSameAs x.map(f)".law(MAP.!)

"""
val x0 = x
x0.`transform`((a,b) => f((a,b))._2)
x0 theSameAs x.map{ case (a,b) => a -> f((a,b))._2 }
""".law(MAP)

"{ val x0 = x; x0.`trimEnd`(n); x0 theSameAs x.`dropRight`(n) }".law

"{ val x0 = x; x0.`trimStart`(n); x0 theSameAs x.`drop`(n) }".law

"x.`updated`(n,a) theSameAs (x.`take`(n).`:+`(a).`++`(x.`drop`(n+1)))".law(SEQ)

"x.`updated`(a._1, a._2).`forall`{ case (k,v) => if (k == a._1) v == a._2 else x.`get`(k).exists(_ == v) }".law(MAP)

// n a !RANGE !M ... n < 0 || n >= x.`size` || sameType(x, x.`updated`(n,a))

// a M !SI8814 ... sameType(x, x.`updated`(a._1, a._2))

"{ val x0 = x; x0.`update`(n, a); x0 theSameAs x.`updated`(n,a) }".law(MAP.!)

"{ val x0 = x; x0.`update`(a._1, a._2); x0 theSameAs x.`updated`(a._1, a._2) }".law(MAP)

"x.`&`(y) isPartOf x".law

"x.`forall`(xi => y.`contains`(xi) == x.`&`(y).contains(xi))".law

// y !RANGE ... sameType(x, x.`&`(y))

"x.`&~`(y) isPartOf x".law

"x.`forall`(xi => y.`contains`(xi) != x.`&~`(y).contains(xi))".law

// y !RANGE ... sameType(x, x.`&~`(y))


////////////////////////////
// End of individual laws //
////////////////////////////

  val complete = b.result
  val all = complete.filterNot(_.disabled)
  lazy val byLineNumber = complete.map(law => law.lineNumber -> law).toMap
}
