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

case class Provider[A, CC](a: A, x: () => CC, xsize: Int, y: () => CC, ysize: Int) {}

abstract class Coll[A, B, CC](val v: Values, val coll: Provider[A, CC])(implicit file: sourcecode.File, line: sourcecode.Line) 
extends Sourced {
  /** Arbitrary element of the type in the collection */
  def a: A = coll.a

  /** An arbitrary element of a type not in the basic collection.  Should match g(a). */
  def b: B

  /** Some function that preserves the type of the elements */
  def f: A => A

  /** Some function that changes the type of the elements.  Note that g(a) == b should be true. */
  def g: A => B

  /** Some integer.  May be positive or negative.  Could really be anything. */
  def L: Int = v.L

  /** A fixed number that is no bigger than the length of `y` */
  def m: Int = if (v.m < 0) (-v.m-1) % ysize else v.m % ysize

  /** A fixed positive number that may be bigger than the length of `y` **/
  val mm: Int = if (v.mm < 0) -v.mm-1 else v.mm

  /** A fixed number that is no bigger than the length of `x` */
  def n: Int = if (v.n < 0) (-v.n-1) % xsize else v.n % xsize

  /** A fixed number that may be bigger than the length of `x` */
  def nn: Int = if (v.nn < 0) -v.nn-1 else v.nn

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
  def x: CC = coll.x()

  /** The known size of the collection `x` */
  def xsize: Int = coll.xsize

  /** Another specific collection, not necessarily the same as `x` */
  def y: CC = coll.y()

  /** The known size of collection `y` */
  def ysize: Int = coll.ysize

  /** Element of type `A` that is a zero with respect to `op`, if `op` exists and a zero exists */
  def zero: () => A = Coll.noZero

  /** Tests whether this Coll has a binary operation defined */
  def hasOp = (op ne Coll.noOp)

  /** Tests whether this Coll has a zero defined */
  def hasZero = (zero ne Coll.noZero) && hasOp

  /** Another coll (or other code source), if any exists, that was used to help generate this one */
  def origin: Option[Sourced]

  /** The location (file and line) where this collection was defined, as a `String` */
  def localize: String = (new java.io.File(file.value)).getName + ", line " + line.value
}
object Coll {
  val noOp: (Any, Any) => Nothing = (a: Any, aa: Any) => throw new IllegalArgumentException("No binary operation defined")
  val noZero: () => Nothing = () => throw new IllegalArgumentException("No zero defined")
}

