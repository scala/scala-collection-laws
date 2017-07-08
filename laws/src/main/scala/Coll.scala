package laws

trait Sourced {
  def localize: String
  def origin: Option[Sourced]
  final def source: String = 
    localize + 
    (origin match {
      case None => ""
      case Some(s) => s.source.split("\n").map("  " + _).mkString("\n", "\n", "")
    })
}

case class Values(L: Int, m: Int, mm: Int, n: Int, nn: Int) {}

abstract class Coll[A, B, CC](val v: Values)(implicit file: sourcecode.File, line: sourcecode.Line) 
extends Sourced {
  /** Arbitrary element of the type in the collection */
  def a: A

  /** An arbitrary element of a type not in the basic collection.  Should match g(a). */
  def b: B

  /** Some function that preserves the type of the elements */
  def f: A => A

  /** Some function that changes the type of the elements.  Note that g(a) == b should be true. */
  def g: A => B

  /** Some integer.  May be positive or negative.  Could really be anything. */
  def L: Int = v.L

  /** A fixed number that is no bigger than the length of `y` */
  def m: Int = if (v.m < 0) (-v.mm-1) % xsize

  /** A fixed positive number that may be bigger than the length of `y` **/
  val mm: Int = if (v.mm < 0) -v.mm-1 else v.mm

  /** A fixed number that is no bigger than the length of `x` */
  def n: Int = n.L

  /** A fixed number that may be bigger than the length of `x` */
  def nn: Int = nn.L

  /** A binary operation on the collection elements.
    *
    * If no such operation exists, leave it as `Coll.noOp` and tests requiring this operator will be skipped.
    */
  def op: (A, A) => A = Coll.noOp // A binary operation, hopefully commutative

  /** A predicate on the type of the elements */
  def p: A => Boolean

  /** A partial function that preserves the type of the elements */
  def pf: PartialFunction[A, A]   

  /** A specific collection of some size */
  def x: CC

  /** The known size of the collection `x` */
  def xsize: Int

  /** Another specific collection, not necessarily the same as `x` */
  def y: CC

  /** The known size of collection `y` */
  def ysize: Int

  /** Element of type `A` that is a zero with respect to `op`, if `op` exists and a zero exists */
  def zero: () => A = Coll.noZero

  /** Tests whether this Coll has a binary operation defined */
  def hasOp = (op ne Coll.noOp)

  /** Tests whether this Coll has a zero defined */
  def hasZero = (op ne Coll.noZero) && hasOp

  /** Another coll (or other code source), if any exists, that was used to help generate this one */
  def origin: Option[Sourced]

  /** The location (file and line) where this collection was defined, as a `String` */
  def localize: String = (new java.io.File(file.value)).getName + ", line " + line.value
}
object Coll {
  val noOp: (Any, Any) => Nothing = (a: Any, aa: Any) => throw new IllegalArgumentException("No binary operation defined")
  val noZero: () => Nothing = () => throw new IllegalArgumentException("No zero defined")
}

abstract class PrimColl[CC](v: Values)(implicit file: sourcecode.File, line: sourcecode.Line)
extends Coll[Int, Long, CC](v)(file, line) {
  protected def transform(i: Int): Long = i.toLong + 3
  lazy val b: Long = transform(a)
  lazy val g: Int => Long = transform _
}

abstract class ObjColl[CC](v: Values)(implicit file: sourcecode.File, line: sourcecode.Line)
extends Coll[String, Option[String], CC](v)(file, line) {
  protected def transform(s: String): Option[String] = {
    val i = a
    if ((i eq null) || (i.length < 2)) None else Some(i.substring(1))
  }
  lazy val b: Option[String] = transform(a)
  lazy val g: String => Long = transform _
}

trait IntFA { val f: Int => Int = _ + 1 }
trait IntFB { val f: Int => Int = (i: Int) => (i*i) - 3*i + 1 }
trait IntOpA { val op: (Int, Int) => Int = _ + _ }
trait IntOpB { val op: (Int, Int) => Int = (i: Int, j: Int) => i*j - 2*i - *j + 4 }

trait StrFA { val f: String => String = (s: String) => if (s ne null) s.toUpperCase else s }
trait StrFB { val f: String => String = (s: String) => if (s ne null) f"<$s<" else s }
trait StrOpA {
  val op: (String, String) => String = (s: String, t: String) => {
    if (s eq null) {
      if (t eq null) s else t
    }
    else if (t eq null) s else s + t
  }
}
trait StrOpB {
  val op (String, String) => String = (s: String, t: String) => {
    if ((s eq null) && (t eq null)) s
    else if (s eq null) t.reverse
    else if (t eq null) s.toUpperCase
    else s.take(t.length) + t.take(s.length).reverse
  }
}
