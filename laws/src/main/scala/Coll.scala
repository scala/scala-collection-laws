package laws

/** Trait that captures the idea of having a source from which one is generated.
  */
trait Sourced {
  /** A text description of where to find this particular instance */
  def localize: String

  /** A text description of where to find the generator used to create this instance */
  def origin: Option[Sourced]

  /** A text description containing all known location information. */
  final def source: String = 
    localize + 
    (origin match {
      case None => ""
      case Some(s) => s.source.split("\n").map("  " + _).mkString("\n", "\n", "")
    })
}

/** Sets of integer values used to check individual cases in certain tests.
  *
  * In general, we expect but do not enforce here that `m` and `n` are positive
  * and bounded by the size of collections `x` and `y` (in the `Coll` in which
  * these `Values` are used), and that `mm` and `nn` are both positive.  `L` can
  * be anything.
  *
  * The `Count` methods are used to detect whether a variable is used in a test or not.
  * If not, alternate values of that variable are not tested.  Note that laws are
  * expected to be deterministic, so this should always be a safe strategy.
  */
class Values private (private val L0: Int, private val m0: Int, private val mm0: Int, private val n0: Int, private val nn0: Int) {
  private[this] val _LCount: Int = 0
  private[this] val _mCount: Int = 0
  private[this] val _mmCount: Int = 0
  private[this] val _nCount: Int = 0
  private[this] val _nnCount: Int = 0

  def L: Int = { _LCount += 1; L0 }
  def m: Int = { _mCount += 1; m0 }
  def mm: Int = { _mmCount += 1; mm0 }
  def n: Int = { _nCount += 1; n0 }
  def nn: Int = { _nnCount += 1; nn0 }

  def LCount: Int = _LCount
  def mCount: Int = _mCount
  def mmCount: Int = _mmCount
  def nCount: Int = _nCount
  def nnCount: Int = _nnCount

  override def equals(that: Any) = that match {
    case v: Values => L0 == v.L0 && m0 == v.m0 && mm0 == v.mm0 && n0 == v.n0 && nn0 == v.nn0
    case _ => false
  }
  override def hashCode = ???

  }
  override def toString = f"Values($L0, $m0, $mm0, $n0, $nn0)"
}
object Values {
  def apply(L: Int, m: Int, mm: Int, n: Int, nn: Int): Values = new Values(L, m, mm, n, nn)
}

/** A provider of collections of a particular type with a particular element type.
  * Each call to the methods should return the same collection; if mutable, the instance
  * should be created afresh each time.  If immutable, it shouldn't matter.
  * (Laws that check reference identity should cache the value.)
  *
  * That `A` is actually an element that can be found within `CC` is not enforced.
  */
class Provider[A, CC] private (a0: A, x0: () => CC, xsize0: Int, y0: () => CC, ysize0: Int) {
  private[this] val _aCount: Int = 0
  private[this] val _xCount: Int = 0
  private[this] val _xsizeCount: Int = 0
  private[this] val _yCount: Int = 0
  private[this] val _ysizeCount: Int = 0

  /** An example element of a type that can be found within the collection */
  def a: A = { _aCount += 1; a0 }

  /** A particular instance of a collection. */
  def x: CC = { _xCount += 1; x0() }

  /** The size of the collection `x` */
  def xsize: Int = { _xsizeCount += 1; xsize0 }

  /** Another instance of a collection, which may or may not be the same as `x` */
  def y: CC = { _yCount += 1; y0() }

  /** The size of the collection `y` */
  def ysize: Int = { _ysizeCount += 1; ysize0 }

  def aCount: Int = _aCount
  def xCount: Int = _xCount
  def xsizeCount: Int = _xsizeCount
  def yCount: Int = _yCount
  def ysizeCount: Int = _ysizeCount
}
object Provider {
  def apply[A, CC](a: A)(x: => CC, xsize: Int)(y: => CC, ysize: Int): Provider[A, CC] =
    new Provider(a, () => x, xsize, () => y, ysize)
}


/** The abstraction of a collection to be tested: this provides all elements and collections
  * and mappings thereon which are available inside the tests.
  */
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

  /** A fixed positive number that is no bigger than the length of `y` */
  def m: Int = if (v.m < 0) (-v.m-1) % ysize else v.m % ysize

  /** A fixed positive number that may be bigger than the length of `y` **/
  val mm: Int = if (v.mm < 0) -v.mm-1 else v.mm

  /** A fixed positive number that is no bigger than the length of `x` */
  def n: Int = if (v.n < 0) (-v.n-1) % xsize else v.n % xsize

  /** A fixed positive number that may be bigger than the length of `x` */
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

