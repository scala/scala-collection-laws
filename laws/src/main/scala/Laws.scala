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
  def filter(p: TestInfo => Boolean): Law =new Law(name, tags filter p, code, disabled)(file, line)

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
  implicit class LawMaker(text: String)(implicit file: sourcecode.File, line: sourcecode.Line) {
    /** Used to force implicit resolution of this class if it's otherwise ambiguous */
    def make: this.type = this

    /** This law should not presently be used */
    def skip {
      b += new Law("", Tags.empty, text, disabled = true)
    }

    /** A law with no tags */
    def law { 
      b += new Law(text)
    }

    /** A law with tags */
    def law(tags: Tags.Taggish, more: Tags.Taggish*) {
      b += new Law(Tags(tags, more: _*), text)
    }

    /** A named law with no tags */
    def named(name: String) {
      b += new Law(name, text)
    }

    /** A named law with tags */
    def named(name: String, tags: Tags.Taggish, more: Tags.Taggish*) {
      b += new Law(name, Tags(tags, more: _*), text)
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

"x.map(f) theSameAs { val y = collection.mutable.HashSet.empty[A]; x.foreach(y += _); y.map(f) }".law(SEQ)

/** TODO ***
"sameType(x, x.map(f))".law("selfs")
*** TODO **/

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

/** TODO ***
"sameType(x, x.flatMap(xi => y.toList.take($INT(xi))))".law("selfs")
*** TODO **/

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

/******** TODO: port these from old-style laws *********
// Make sure types are propagated both explicitly and when downcast to a supertype
{ val xx = x.`genericBuilder`[@A]; x.foreach{ xx += _ }; x theSameAs xx.result }
!RANGE !SORTED !ARRAY !USESARRAY !N ... { val xx = x.`genericBuilder`[@A]; sameType(x, xx.result) }
// Combine the following two lines into one when set issues are fixed
!RANGE !SORTED !ARRAY !USESARRAY !VIEW !S ... type ROOT[TARG] = @ROOTS[TARG]; val y: Any = x; y match { case r: ROOT[_] => val xx = r.asInstanceOf[ROOT[@A]].map(identity); classOf[@CC] isAssignableFrom xx.getClass; case _ => true }
!RANGE !SORTED !ARRAY !USESARRAY !VIEW S !M FIXED8434 ... type ROOT[TARG] = @ROOTS[TARG]; val y: Any = x; y match { case r: ROOT[_] => val xx = r.asInstanceOf[ROOT[@A]].map(identity); classOf[@CC] isAssignableFrom xx.getClass; case _ => true }
!RANGE !SORTED !ARRAY !VIEW M FIXED8434 ... type ROOT[TARG1,TARG2] = @ROOTMAPS[TARG1,TARG2]; val y: Any = x; y match { case m: ROOT[_, _] => val xx = m.asInstanceOf[ROOT[@K, @V]].map(identity); classOf[@CC] isAssignableFrom xx.getClass; case _ => true }
********* END TODO ********/

/******** Laws that should apply to most collections ********/

"x.`aggregate`(zero)((b,a) => b, (b1,b2) => b1) == zero".law

"x.`aggregate`(zero)((b,a) => b, (b1,b2) => b2) == zero".law

/******** TODO--port the rest of these *********
!SI7128 !S n m r ... n < 0 || { val arr = new Array[@A](r); x.`copyToArray`(arr, n, m); (arr.drop(n).take(m) zip x.toList).forall{ case (x,y) => x==y } }
!SI7128 S n m r ... n < 0 || { val arr = new Array[@A](r); x.`copyToArray`(arr, n, m); val c = arr.drop(n).take(m min x.`size`);  val cs = c.toSet; c.size == cs.size && (cs subsetOf x.toSet) }
pf ... x.`collectFirst`(pf).isDefined == x.`exists`(pf.isDefinedAt)
val b = new collection.mutable.ArrayBuffer[@A]; x.`copyToBuffer`(b); b.result theSameAs x
x theSameAs x.`companion`(x.toList: _*)
!RANGE !SORTED !ARRAY !USESARRAY !N ... sameType(x, x.`companion`.empty[@A])
***********************************************/

"x.`count`(p) > 0 == x.`exists`(p)".law

"(x.`count`(p) == x.`size`) == x.`forall`(p)".law

"x.`count`(p) == { var k=0; x.`foreach`(xi => if (p(xi)) k += 1); k }".law

"x.filter(p).`size` == x.`count`(p)".law

"x.filter(p).`forall`(p) == true".law

/******** TODO--port the rest of these *********
p !RANGE ... sameType(x, x.filter(p))

// Why doesn't array work?!  Induces out-of-place ambiguous implicit conversion problem on for loop (booleanArrayOps & byteArrayOps)?!
y !ARRAY !M ... x.flatMap(xi => y.toList.take($INT(xi))) theSameAs x.map(xi => y.toList.take($INT(xi))).`flatten`
***********************************************/

"x.`foldLeft`(a)(op) == x.`foldRight`(a)(op)".law

"x.`foldLeft`(a)(op) == x.`/:`(a)(op)".law

"""x.`foldRight`(a)(op) == x.`:\`(a)(op)""".law

/******** TODO--port the rest of these *********
!ADEFINITE ... x.`nonEmpty` implies (x.`hasDefiniteSize` == @DEFINITE)
x.`isTraversableAgain` == @AGAIN
***********************************************/

"tryO{x.`max`} == tryO{ x.`reduce`(maxOf) }".law

"tryO{x.`maxBy`(f)} == tryO{ val fx = x.map(f).`max`; x.find(xi => f(xi)==fx).get }".law

"tryO{x.`min`} == tryO{ x.`reduce`(minOf) }".law

"tryO{x.`minBy`(f)} == tryO{ val fx = x.map(f).`min`; x.find(xi => f(xi)==fx).get }".law

"x.`nonEmpty` == x.`exists`(_ => true)".law


/******** TODO--port the rest of these *********
one ... x.`product` == x.`fold`(one)(_ * _)
zero ... x.`sum` == x.`fold`(zero)(_ + _)
***********************************************/

"""Set(
  tryO{x.`reduce`(op)}, tryO{x.`reduceLeft`(op)}, tryO{x.`reduceRight`(op)},
  x.`reduceOption`(op), x.`reduceLeftOption`(op), x.`reduceRightOption`(op)
).size == 1""".law(select(_.isSymOp))

"x.`size` == x.`count`(_ => true)".law

"x theSameAs x.`to`[List]".law

"x.`toArray` theSameAs x".law

"x.`toBuffer` theSameAs x".law

"x.`toIndexedSeq` theSameAs x".law

"x.`toIterable` theSameAs x".law

"x.`toList` theSameAs x".law

"x.map(xi => (xi,xi)).`toMap` theSameAs { val hm = new collection.mutable.HashMap[A,A]; x.foreach(xi => hm += xi -> xi); hm }".law

"x.`toSeq` theSameAs x".law

"x.`toSet` isPartOf x".law

"x.`toVector` theSameAs x".law


/******** TODO--port the rest of these *********
p !S ... val y = new collection.mutable.ArrayBuffer[@A]; x.`withFilter`(p).foreach(y += _); y theSameAs x.filter(p)
p S ... val y = new collection.mutable.HashSet[@A]; x.`withFilter`(p).foreach(y += _); y theSameAs x.filter(p)
x.`hasNext` == x.`nonEmpty`
x.`hasNext` implies tryO{ x.`next` }.isDefined
y !S ... x.`++`(y).`size` == x.size + y.size
y S ... x.`++`(y).`size` <= x.size + y.size
y S !M ... x.`++`(y).`size` == x.size + y.filter(yi => !x.`contains`(yi)).size
y S M ... x.`++`(y).`size` == x.size + y.filter(yi => !x.`contains`(yi._1)).size
y !S ... (x.`++`(y)).`take`(x.`size`) theSameAs x
y !S ... (x.`++`(y)).`drop`(x.`size`) theSameAs y
y !M ... x.`--`(y).`size` >= x.size - y.size
y M ... x.`--`(y.map(_._1)).`size` >= x.size - y.size
y !M ... (x.`--`(y)) isPartOf x
y M ... x.`--`(y.map(_._1)) isPartOf x
y !M ... val List(a,b,c) = List(x, y, x.`--`(y)).map(_.groupBy(identity).mapValues(_.size)); a.forall{ case (k,v) => v == b.getOrElse(k,0) + c.getOrElse(k,0) }
// Need -- that works for maps
y !RANGE !MUVU !SI8814 ... sameType(x, x.`++`(y))
x.`buffered` theSameAs x
pf ... x.`collect`(pf) theSameAs x.`filter`(pf.isDefinedAt).`map`(pf)
pf !RANGE !MUVU !SI6462 ... sameType(x, x.`collect`(pf))
a !M ... x.`contains`(a) == x.`exists`(_ == a)
a M ... x.`contains`(a._1) == x.`exists`(_._1 == a._1)
x.`corresponds`(x)(_ == _)
y ... (x.`size` != y.size) implies !x.`corresponds`(y)(_ == _)
y ... x.`corresponds`(y)((_,_) => false) implies !x.`nonEmpty` && !y.`nonEmpty`
n ... x.`drop`(n).`size` == (0 max (x.size-(0 max n)))
n ... x.`drop`(n) isPartOf x
n ... sameType(x, x.`drop`(n))
p !S ... val y = x.`dropWhile`(p); y.`take`(1).toList match { case Nil => true; case List(xi) => !p(xi); case _ => false }
p S ... val y = x.`dropWhile`(p); y.nonEmpty implies y.exists(yi => !p(yi))
p ... x.`dropWhile`(p) isPartOf x
p ... sameType(x, x.`dropWhile`(p))
val (x1,x2) = x.`duplicate`; x1.`corresponds`(x)(_ == _) && x2.corresponds(x)(_ == _)
p ... val (x1,x2) = x.`duplicate`; sameType(x1, x2)
p ... x.`filterNot`(p) theSameAs x.`filter`(xi => !p(xi))
p !RANGE ... sameType(x, x.`filterNot`(p))
n ... n <= 0 || x.`grouped`(n).map(_.size).`sum` == x.`size`
n m ... n <= 0 || m < 0 || { var y = 0; x.`grouped`(n).`drop`(m).take(1).map(_.`size`).foreach(y = _); (y < n) implies !x.grouped(n).drop(m+1).`nonEmpty` }
n ... x.`grouped`((1 max n)).flatMap(xi => xi) theSameAs x
x.`isEmpty` == !x.`nonEmpty`
x.`length` == x.`size`
a n ... x.`padTo`(n, a).`size` == (n max x.size)
a n ... x.`padTo`(n, a).`drop`(x.`size`).`forall`(_ == a)
a n ... (n <= x.`size`) implies (x.`padTo`(n, a) theSameAs x)
a n !RANGE !MUVU ... sameType(x, x.`padTo`(n,a))
p ... val (t,f) = x.`partition`(p); (t theSameAs x.`filter`(p)) && (f theSameAs x.`filterNot`(p))
p !RANGE ... val (t,f) = x.`partition`(p); sameType(t,f) && sameType(x,t)
y n m ... (n <= x.`size`) implies (x.`patch`(n, y, m).`take`(n) theSameAs x.take(n))
y n m ... (n <= x.`size`) implies (x.`patch`(n, y, m).`drop`(n).`take`(y.`size`) theSameAs y)
y n m ... (n <= x.`size`) implies (x.`patch`(n, y, m).`drop`((0 max n)+y.`size`) theSameAs x.`drop`((0 max n)+(0 max m)))
y ... (x `sameElements` y) == (x theSameAs y)
n m ... n < 0 || m >= x.size || { x.`slice`(n, m).`size` == (0 max ((0 max m)-n)) }
n m !S ... n < 0 || m >= x.size || { x.`slice`(n, m) theSameAs x.`drop`(n).`take`((0 max m)-n) }
n m !SI8819 ... n < 0 || m >= x.size || { sameType(x, x.`slice`(n, m)) }
p ... x.`span`(p)._1.`forall`(p)
p !S ... (x.span(p)._2.`size`) > 0 implies !x.`span`(p)._2.`take`(1).`exists`(p)
p S ... (x.span(p)._2.`size`) > 0 implies !x.`span`(p)._2.`forall`(p)
p ... val (x1, x2) = x.`span`(p); val n = x.`span`(p)._1.`size`; (x1 theSameAs x.`take`(n)) && (x2 theSameAs x.`drop`(n))
p ... val (x1, x2) = x.`span`(p); sameType(x1, x2) && sameType(x, x1)
n ... x.`take`(n).`size` == ((0 max n) min x.size)
n ... x.`take`(n) isPartOf x
n ... sameType(x, x.`take`(n))
p ... x.`takeWhile`(p).`forall`(p)
p ... x.`takeWhile`(p) isPartOf x
p ... x.`takeWhile`(p).`size` + x.`dropWhile`(p).size == x.size
p ... sameType(x, x.`takeWhile`(p))
y ... x.`zip`(y).map(_._1) theSameAs x.take(x.size min y.size)
y ... x.`zip`(y).map(_._2) theSameAs y.take(x.size min y.size)
y !N !MUVU !SI6462 ... sameType(x, x.`zip`(y).map(_._1))
y a b ... x.`zipAll`(y, a, b).map(_._1) theSameAs x.`padTo`(x.`size` max y.size, a)
y a b ... x.`zipAll`(y, a, b).map(_._2) theSameAs y.`padTo`(x.`size` max y.size, b)
y a b !N !MUVU !SI6462 ... sameType(x, x.`zipAll`(y, a, b).map(_._1))
x.`zipWithIndex`.map(_._1) theSameAs x
x.`zipWithIndex`.map(_._2) theSameAs (0 until x.`size`)
!N !MUVU !SI6462 ... sameType(x, x.`zipWithIndex`.map(_._1))
y ... x.`:++`(y) theSameAs x.`++`(y)
y ... x.`++:`(y) theSameAs y.`++`(x)
a ... x.`::`(a).`size` == x.size+1
a ... x.`::`(a).`head` == a
a ... sameType(x, x.`::`(a))
a ... x.`+:`(a).`size` == x.size+1
a ... x.`+:`(a).`head` == a
a !RANGE !MUVU ... sameType(x, x.`+:`(a))
a ... x.`:+`(a).`size` == x.size+1
a ... x.`:+`(a).`last` == a
a !RANGE !MUVU ... sameType(x, x.`:+`(a))
a ... val s = x.`+`(a).`size` - x.size; 0 <= s && s <= 1
a !M ... x.`+`(a).`contains`(a)
a M ... x.`+`(a).`contains`(a._1)
a !RANGE !MUVU !SI8814 ... sameType(x, x.`+`(a))
y ... x.`:::`(y) theSameAs y.`++`(x)
y ... sameType(x, x.`:::`(y))
n !ARRAY !M ... (n < 0) || (n >= x.`size`) || ((x: Any) match { case _: scala.collection.Seq[_] => Seq(x.`apply`(n)) theSameAs x.`drop`(n).`take`(1); case _ => true })
n ARRAY ... (n < 0) || (n >= x.`size`) || { Seq(x.`apply`(n)) theSameAs x.`drop`(n).`take`(1) }
r ... x.`size` > 8 || { val n = x.`combinations`(r).size; ((r > x.size) implies (n == 0)) && x.combinations(r).toSet.size == n }
r ... x.`size` > 8 || r == 0 || r > x.size || { val s = x.toVector; x.`combinations`(r).forall(_ isPartOf s) }
y ... x.`containsSlice`(y) implies (y.`size` == 0 || (Iterator.from(0).map(x.`drop`).takeWhile(_.`nonEmpty`).exists(_.`take`(y.`size`) theSameAs y)))
y ... val List(xg,yg,xdyg) = List(x,y,x.`diff`(y)).map(_.`groupBy`(identity).mapValues(_.size).toMap); xg.forall{ case (k,n) => val m = n - yg.getOrElse(k,0); (m > 0) implies (xdyg.getOrElse(k,0) == m) }
val s = x.`toSet`; x.`distinct`.`size` == s.size && x.forall(s)
nx !S ... x.`dropRight`(nx) theSameAs x.`take`(x.`size` - math.max(0, nx))
sameType(x, x.`reverse`)
y ... x.`endsWith`(y) == (x.`drop`(math.max(0, x.`size`-y.size)) theSameAs y)
g ... x.`groupBy`(g).keySet theSameAs x.map(g).toSet
g ... x.`groupBy`(g).toMap.forall{ case (k,vs) => x.filter(xi => g(xi)==k) theSameAs vs }
!ARRAY !M ... !x.`nonEmpty` || (x match { case _: scala.collection.Seq[_] => Seq(x.`head`) theSameAs x.`take`(1); case _ => true })
ARRAY ... !x.`nonEmpty` || (Seq(x.`head`) theSameAs x.`take`(1))
x.`headOption` == tryO{ x.`head` }
n a !ITERATOR ... n < 0 || { val k = x.`drop`(n).`takeWhile`(_ != a); x.`indexOf`(a,n) match { case -1 => k.`size` == (0 max x.size-n); case kk => n+k.size == kk && x.`drop`(kk).`head` == a } }
a !ITERATOR ... x.`indexOf`(a) == x.`indexOf`(a,0)
a ITERATOR ... { val k = x.`takeWhile`(_ != a); x.`indexOf`(a) match { case -1 => k.`size` == x.size; case kk => x.`drop`(kk).`next` == a } }
y ... y.`size` == 0 || { val i = x.`indexOfSlice`(y); !x.`take`(math.max(0,i-1+y.size)).`containsSlice`(y) && ((i >= 0) implies (x.`drop`(i).`take`(y.size) theSameAs y)) }
y r ... y.`size` == 0 || { val i = x.`indexOfSlice`(y,r); !x.`drop`(r).`take`(math.max(0,i-1-r+y.size)).`containsSlice`(y) && ((i >= 0) implies (i >= r && (x.`drop`(i).`take`(y.size) theSameAs y))) }
p n !ITERATOR ... n < 0 || { val k = x.`drop`(n).`takeWhile`(xi => !p(xi)); x.`indexWhere`(p,n) match { case -1 => k.`size` == (0 max x.size-n); case kk => n+k.size == kk && p(x.`drop`(kk).`head`) } }
p !ITERATOR ... x.`indexWhere`(p) == x.`indexWhere`(p,0)
p ITERATOR ... val k = x.`takeWhile`(xi => !p(xi)).`size`; x.`indexWhere`(p) match { case -1 => k == x.`size`; case q => q == k }
x.`indices` == (0 until x.`size`)
x.`size` == 0 || (x.`init` theSameAs x.`dropRight`(1))
x.`size` == 0 || (x.`init`.size == x.size - 1)
x.`size` == 0 || (x.`init` isPartOf x)
x.`size` == 0 || { val xx = x.`inits`.toVector.map(_.toVector); (xx zip xx.`tail`).`forall`{ case (a,b) => a.`size` - 1 == b.size && (b isPartOf a) } }
!S ... { val xx = x.`inits`.toVector.map(_.toVector); (xx zip xx.`drop`(1)).`forall`{ case (a,b) => a.`init` theSameAs b } }
y ... x.`intersect`(y).`toSet` == (x.toSet & y.toSet)  // Need to improve this one
y !RANGE ... sameType(x, x.`intersect`(y))
x.`iterator` theSameAs x
x.`size` == 0 || { Seq(x.`last`) theSameAs x.`takeRight`(1) }
a n ... n < 0 || { x.`lastIndexOf`(a,n) match { case -1 => x.`take`(n+1).`indexOf`(a) == -1; case k => k == (n min x.`size`-1) - x.take(n+1).reverse.indexOf(a) } }
a ... x.`lastIndexOf`(a) == x.lastIndexOf(a,x.`size`-1)
y ... y.`size` == 0 || { val i = x.`lastIndexOfSlice`(y); !x.`drop`(math.max(0,i+1)).`containsSlice`(y) && ((i >= 0) implies (x.`drop`(i).`take`(y.size) theSameAs y)) }
y r ... y.`size` == 0 || { val i = x.`lastIndexOfSlice`(y,r); !x.`take`(r).`drop`(math.max(0,i+1)).`containsSlice`(y) && ((i >= 0) implies (i <= r && (x.`drop`(i).`take`(y.size) theSameAs y))) }
p n ... n < 0 || { x.`lastIndexWhere`(p,n) match { case -1 => x.`take`(n+1).`indexWhere`(p) == -1; case k => k == (n min x.`size`-1) - x.take(n+1).reverse.indexWhere(p) } }
p ... x.`lastIndexWhere`(p) == x.lastIndexWhere(p,x.`size`-1)
x.`lastOption` == tryO{ x.`last` }
n ... x.`lengthCompare`(n).signum == (x.`size` compare n).signum
!ITERATOR ... val xx = x.map(_.toString); xx eq xx.`mapConserve`(identity)
x.`size` > 8 || x.`permutations`.size == x.toVector.groupBy(identity).map{ case (_,vs) => vs.size }.scanLeft((x.size,0)){ (c,i) => (c._1 - c._2, i) }.map{ case (n,k) => (1L to k).map(n+1 - _).product / (1L to k).product }.product
x.`size` > 8 || x.`permutations`.size == x.`permutations`.toSet.size
x.`size` > 8 || { val xs = x.toSet; x.`permutations`.forall(_.forall(xs)) }
p ... x.`prefixLength`(p) == x.`takeWhile`(p).`size`
val ix = x.`toIndexedSeq`; val k = x.`size`; var ki = 0; x.`reverse`.`forall`{ xi => ki += 1; xi == ix(k - ki) }
x.`reverseIterator` theSameAs x.`reverse`
f ... x.`reverseMap`(f) theSameAs x.`reverse`.map(f)
f ... x.`reverseMap`(f) theSameAs x.map(f).`reverse`
f !RANGE !MUVU ... sameType(x, x.`reverseMap`(f))
y ... x.`reverse_:::`(y) theSameAs x.`:::`(y.`reverse`)
y ... sameType(x, x.`reverse_:::`(y))
op !S one ... x.`scan`(one)((a,b) => op(a,b)) theSameAs x.toList.scanLeft(one)((a,b) => op(a,b))
op !S one ... x.`scanLeft`(one)((a,b) => op(a,b)) theSameAs { val ab = collection.mutable.ArrayBuffer(one); x.foreach(xi => ab += (op(ab.last, xi))); ab }
op !S one ... x.`scanRight`(one)((a,b) => op(a,b)) theSameAs x.toList.reverse.scanLeft(one)((a,b) => op(a,b)).reverse
S ... { val y = x.`scan`("")((s,xi) => s.toString + xi.toString).toList.sortBy(_.toString.length); (y zip y.tail).forall{ case (a,b) => b.toString.startsWith(a.toString) } }
S ... x.`scan`("")((s,xi) => s.toString + xi.toString).toList.sortBy(_.toString.length).lastOption.forall{ yN => yN.toString.groupBy(identity) == x.map(_.toString).mkString.groupBy(identity) }
S ... { val y = x.`scanLeft`(Set[@A]())((s,xi) => s + xi).toList.sortBy(_.size); (y zip y.tail).forall{ case (a,b) => a subsetOf b} }
S ... x.`scanLeft`(Set[@A]())((s,xi) => s + xi).toList.sortBy(_.size).lastOption.forall(_ == x.toSet)
S ... { val y = x.`scanRight`(Set[@A]())((xi,s) => s + xi).toList.sortBy(_.size); (y zip y.tail).forall{ case (a,b) => a subsetOf b} && y.lastOption.forall(_ == x.toSet) }
S ... x.`scanRight`(Set[@A]())((xi,s) => s + xi).toList.sortBy(_.size).lastOption.forall(_ == x.toSet)
p n ... n <= 0 || x.`segmentLength`(p,n) == x.`drop`(n).`takeWhile`(p).`size`
r ... r <= 0 || x.`sliding`(r).`size` == (if (x.nonEmpty) math.max(0,x.`size`-r)+1 else 0)
r !S ... r <= 0 || x.`size` <= 0 || (r >= x.`size` && { x.`sliding`(r).map(_.toVector).toVector == Vector(x.toVector) }) || { val vx = x.toVector; x.`sliding`(r).toVector.map(_.toVector) == Vector.range(0, 1+vx.size-r).map(i => vx.slice(i,i+r)) }
f ... x.`sortBy`(f) theSameAs x.`sortWith`((a,b) => f(a) < f(b))
f ... val xx = x.`sortWith`((a,b) => f(a) > f(b)).toList; (xx zip xx.drop(1)).forall{ case (a,b) => !(f(a) < f(b)) }
x.`sorted` theSameAs x.`toArray`.sorted // Need to add a custom ordering here
n ... val (x1,x2) = x.`splitAt`(n); (x1 theSameAs x.`take`(n)) && (x2 theSameAs x.`drop`(n))
n ... val (x1,x2) = x.`splitAt`(n); sameType(x1, x2) && sameType(x, x1)
n y ... n < 0 || { x.`startsWith`(y,n) implies (x.`drop`(n).`take`(y.`size`) theSameAs y) }
y ... x.`startsWith`(y) == x.startsWith(y,0)
x.`size` < 1 || { x.`tail` theSameAs x.`drop`(1) }
!S ... x.`tails`.toList.map(_.toList) match { case x => (x zip x.tail).forall{ case (a :: b :: _, c :: _) => b == c; case (a,b) => b.isEmpty && a.size == 1 } }
S !M ... x.`tails`.toList match { case x => (x zip x.tail).forall{ case (a,b) => (b subsetOf a) && b.size == a.size-1 } }
S M ... x.`tails`.toList match { case x => (x zip x.tail).forall{ case (a,b) => (b.toSet subsetOf a.toSet) && b.size == a.size-1 } }
nx ... x.`takeRight`(nx) theSameAs { val m = x.`size` - math.max(0, nx); x.`drop`(m) }
n !RANGE ... sameType(x, x.`takeRight`(n))
n !RANGE !ARRAY ... n < 1 || x.map(a => List.fill(n)(a)).`transpose`.`forall`(_ theSameAs x)
n !RANGE ARRAY ... n < 1 || x.map(a => Array.fill(n)(a)).`transpose`.`forall`(_ theSameAs x)
y ... x.`union`(y).`toSet` == (x.toSet union y.toSet)
y !RANGE !MUVU ... sameType(x, x.`union`(y))
y ... val xa = x.`zip`(y).`unzip`._1.`toArray`; x.`take`(xa.size) theSameAs xa
y ... val xb = x.`zip`(y).`unzip`._2.`toArray`; y.`take`(xb.size) theSameAs xb
x.map(a => (a,a)).`unzip` match { case (y1, y2) => List(y1, y2).forall(_ theSameAs x); case _ => false }
x.map(a => (a,a,a)).`unzip3` match { case (y1, y2, y3) => List(y1, y2, y3).forall(_ theSameAs x); case _ => false }
y !S ... val z = x.`zip`(y).toArray; val n = math.min(x.`size`, y.size); var i,j = -1; x.`forall`{ xi => i += 1; i >= n || z(i)._1 == xi } && y.forall{ yj => j += 1; j >= n || z(j)._2 == yj }
y S !M ... val z = x.`zip`(y).toArray; (z.map(_._1).toSet subsetOf x) && (z.map(_._2).toSet subsetOf y) && z.forall{ case (a,b) => (x contains a) && (y contains b) }
// Need to properly test Map zip
x theSameAs x.`view`
y ... val x0 = x; x0.`++=`(y); x0 theSameAs (x.`++`(y))
y ... val x0 = x; x0.`++=:`(y); x0 theSameAs (y.`++`(x))
a ... val x0 = x; x0.`+=`(a); x0 theSameAs (x.`:+`(a))
a ... val x0 = x; x0.`+=`(a); x0 theSameAs (x.`+`(a))
a ... val x0 = x; x0.`+=:`(a); x0 theSameAs (x.`+:`(a))
a !M ... (x.`-`(a)).`count`(_ == a) == (0 max x.count(_ == a) - 1)
a M ... (x.`-`(a._1)).`count`(_ == a) == 0
a !M ... (x.`-`(a)).`size` == x.size - (if (x.`contains`(a)) 1 else 0)
a M ... (x.`-`(a._1)).`size` == x.size - (if (x.`contains`(a._1)) 1 else 0)
a !M ... (x.`-`(a)) isPartOf x
a M ... (x.`-`(a._1)) isPartOf x
a !RANGE !M ... sameType(x, x.`-`(a))
a M ... sameType(x, x.`-`(a._1))
a !M ... val x0 = x; x0.`-=`(a) theSameAs x.`-`(a)
a M ... val x0 = x; x0.`-=`(a._1) theSameAs x.`-`(a._1)
y !M ... val x0 = x; x0.`--=`(y); val x1 = x; y.foreach(yi => x1.`-=`(yi)); x0 theSameAs x1
y M ... val x0 = x; x0.`--=`(y.map(_._1)); val x1 = x; for ((k,_) <- y) { x1.`-=`(k) }; x0 theSameAs x1
a ... { val z = x; z.`append`(a); z } == x.`+=`(a)
y ... { val z = x; z.`appendAll`(y); z } == x.`++=`(y)
a ... { val z = x; z.`prepend`(a); z } == x.`+=:`(a)
y ... { val z = x; z.`prependAll`(y); z } == x.`++=:`(y)
n ... n < 0 || n >= x.`size` || { val x0 = x; x0.`reduceToSize`(n); x0.`size` == n }
n !S ... n < 0 || n >= x.`size` || { val x0 = x; x0.`reduceToSize`(n); x0 theSameAs x.`take`(n) }
n m !S !DOUBLL !SI8554 ... tryE{ val x0 = x;  x0.`remove`(n,m); x0 } match { case Left(_: IllegalArgumentException) => m < 0; case Left(_: IndexOutOfBoundsException) => n < 0 || n > (x.`size` - m); case Right(x0) => x0 theSameAs (x.`take`(n).`++`(x.`drop`(n+(0 max m)))); case _ => false }
n m !S !DOUBLL SI8554 ... tryE{ val x0 = x;  x0.`remove`(n,m); x0 } match { case Right(x0) => n < 0 || x.`size` < (n.toLong+m) || (x0 theSameAs (x.`take`(n).`++`(x.`drop`(n+(0 max m))))); case _ => true }
n !S !DOUBLL ... n < 0 || n >= x.`size` || { val x0 = x; val a0 = x0.`remove`(n); x0.`size` == x.`size`-1 && x.`apply`(n) == a0 }
n !S !DOUBLL ... n < 0 || n >= x.`size` || { val x0, x1 = x; x0.`remove`(n); x1.`remove`(n,1); x0 theSameAs x1 }
x.`result` theSameAs x
f !M ... val x0 = x; x0.`transform`(f); x0 theSameAs x.map(f)
f M !ILM ... val x0 = x; x0.`transform`((a,b) => f((a,b))._2); x0 theSameAs x.map{ case (a,b) => a -> f((a,b))._2 }
n ... n < 0 || n >= x.`size` || { val x0 = x; x0.`trimEnd`(n); x0 theSameAs x.`dropRight`(n) }
n ... n < 0 || n >= x.`size` || { val x0 = x; x0.`trimStart`(n); x0 theSameAs x.`drop`(n) }
n a !M ... n < 0 || n >= x.`size` || { x.`updated`(n,a) theSameAs (x.`take`(n).`:+`(a).`++`(x.`drop`(n+1))) }
a M ... x.`updated`(a._1, a._2).`forall`{ case (k,v) => if (k == a._1) v == a._2 else x.`get`(k).exists(_ == v) }
n a !RANGE !M ... n < 0 || n >= x.`size` || sameType(x, x.`updated`(n,a))
a M !SI8814 ... sameType(x, x.`updated`(a._1, a._2))
n a !M ... n < 0 || n >= x.`size` || { val x0 = x; x0.`update`(n, a); x0 theSameAs x.`updated`(n,a) }
a M ... val x0 = x; x0.`update`(a._1, a._2); x0 theSameAs x.`updated`(a._1, a._2)
y ... x.`&`(y) isPartOf x
y ... x.`forall`(xi => y.`contains`(xi) == x.`&`(y).contains(xi))
y !RANGE ... sameType(x, x.`&`(y))
y ... x.`&~`(y) isPartOf x
y ... x.`forall`(xi => y.`contains`(xi) != x.`&~`(y).contains(xi))
y !RANGE ... sameType(x, x.`&~`(y))
********* END TODO ********/


////////////////////////////
// End of individual laws //
////////////////////////////

  val complete = b.result
  val all = complete.filterNot(_.disabled)
  lazy val byLineNumber = complete.map(law => law.lineNumber -> law).toMap
}
