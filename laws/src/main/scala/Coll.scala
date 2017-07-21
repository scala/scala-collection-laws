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
object Sourced {
  /** A text description of the file and line some source came from */
  def local(file: sourcecode.File, line: sourcecode.Line): String =
    (new java.io.File(file.value)).getName + ", line " + line.value

  /** A text description of file and line using implicit values in scope (or the current line) */
  def implicitly(implicit file: sourcecode.File, line: sourcecode.Line): String =
    local(file, line)
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
class Values private (
  private[laws] val L0: Int,
  private[laws] val m0: Int,
  private[laws] val mm0: Int,
  private[laws] val n0: Int,
  private[laws] val nn0: Int
) {
  private[this] var _LCount: Int = 0
  private[this] var _mCount: Int = 0
  private[this] var _mmCount: Int = 0
  private[this] var _nCount: Int = 0
  private[this] var _nnCount: Int = 0

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
  def resetCount: this.type = { _LCount = 0; _mCount = 0; _mmCount = 0; _nCount = 0; _nnCount = 0; this }

  class Secret {
    def L = L0
    def m = m0
    def mm = mm0
    def n = n0
    def nn = nn0
  }
  /** Allow the values to be accessed without recording them (so as not to trigger tests of variants in the parameter) */
  val secret = new Secret

  override def equals(that: Any) = that match {
    case v: Values => L0 == v.L0 && m0 == v.m0 && mm0 == v.mm0 && n0 == v.n0 && nn0 == v.nn0
    case _         => false
  }
  override def hashCode = {
    import scala.util.hashing.MurmurHash3._
    finalizeHash(mixLast(mix(mix(mix(m0, L0), mm0), n0), nn0), 5)
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
class Provider[A, CC] private (
  private[laws] val a0: A,
  private[laws] val x0: () => CC,
  private[laws] val xsize0: Int,
  private[laws] val y0: () => CC,
  private[laws] val ysize0: Int
) {
  private[this] var _aCount: Int = 0
  private[this] var _xCount: Int = 0
  private[this] var _xsizeCount: Int = 0
  private[this] var _yCount: Int = 0
  private[this] var _ysizeCount: Int = 0

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
  def resetCount: this.type = { _aCount = 0; _xCount = 0; _xsizeCount = 0; _yCount = 0; _ysizeCount = 0; this }

  class Secret {
    def a: A = a0
    def x: CC = x0
    def xsize: Int = xsize0
    def y: CC = y0
    def ysize: Int = ysize0
  }
  /** Secretly access the input collections (usage not recorded, so will not cause variants to be run) */
  val secret = new Secret

  override def equals(that: Any) = that match {
    case p: Provider[_, _] => (this eq p) || (a0 == p.a0 && xsize0 == p.xsize0 && ysize0 == p.ysize0 && x0() == p.x0() && y0() == p.y0)
    case _                 => false
  }
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3._
    finalizeHash(mixLast(mix(x0().##, y0().##), a0.##), 1 + xsize0 + ysize0)
  }
  override def toString = {
    def clip(s: String, n: Int) = if (s.length <= n) s else s.substring(0, n-3)+"..."
    f"Provider: singleton $a0 with\n  ${clip(x0().toString, 62)} ; len $xsize0\n  ${clip(y0().toString, 62)} ; len $ysize0"
  }
}
object Provider {
  def apply[A, CC](a: A)(x: => CC, xsize: Int)(y: => CC, ysize: Int): Provider[A, CC] =
    new Provider(a, () => x, xsize, () => y, ysize)
}

/** Wrapper class around a function that lets you tell where it came from */
class ===>[X, Y](val fn: X => Y, desc: String = "")(implicit file: sourcecode.File, line: sourcecode.Line) {
  override val toString =
    if (desc.length > 0) desc + " @ " + Sourced.implictly else Sourced.implicitly
  override def equals(that: Any) = that match {
    case x: _ ===> _ => toString == x.toString
    case _           => false
  }
  override val hashCode = scala.util.hashing.MurmurHash3.stringHash(toString)
}

/** Wrapper class around a binary operation that lets you tell where it came from */
class OpFn[X](val ofn: (X, X) => X, val zero: Option[X], desc: String = "")(implicit file: sourcecode.File, line: sourcecode.Line) {
  override def toString =
    if (desc.length > 0) desc + " @ " + Sourced.implictly else Sourced.implicitly
  override def equals(that: Any) = that match {
    case x: OpFn[_] => toString == x.toString
    case _          => false
  }
  override val hashCode = scala.util.hashing.MurmurHash3.stringHash(toString)
}

/** Wrapper class around a partial function that lets you tell where it came from */
class ParFn[X](val pfn: PartialFunction[X, X], desc: String = "")(implicit file: sourcecode.File, line: sourcecode.Line) {
  override def toString =
    if (desc.length > 0) desc + " @ " + Sourced.implictly else Sourced.implicitly
  override def equals(that: Any) = that match {
    case x: ParFn[_] => toString == x.toString
    case _           => false
  }
  override val hashCode = scala.util.hashing.MurmurHash3.stringHash(toString)
}

class Active[A, B] private (
  private[laws] val f0: A ===> A,
  private[laws] val g0: A ===> B,
  private[laws] val op0: OpFn[A],
  private[laws] val p0: A ===> Boolean,
  private[laws] val pf0: ParFn[A]
) {
  private[this] var _fCount = 0
  private[this] var _gCount = 0
  private[this] var _opzCount = 0
  private[this] var _pCount = 0
  private[this] var _pfCount = 0

  /** A function that changes an element to another of the same type */
  def f: A => A = { _fCount += 1; f0.fn }

  /** A function that changes an element to another of a different type */
  def g: A => A = { _gCount += 1; g0.fn }

  /** A function that, given two elements of a type, produces a single element of that type */
  def op: (A, A) => A = { _opzCount += 1; op0.ofn }

  /** A predicate that gives a true/false answer for an element */
  def p: A => A = { _pCount += 1; p0.fn }

  /** A partial function that changes some elements to another of the same type */
  def pf: PartialFunction[A, A] = { _pfCount += 1; pf0.pfn }

  /** The zero of `op`; throws an exception if there is no zero.  Test using `hasZ`.
    *
    * Note: we're doing it this way since it's not practical to instrument the usage of something inside `Option`.
    */
  val z: A = { _opzCount += 1; op0.zero.get }

  val hasZ: Boolean = op0.zero.isDefined

  def fCount = _fCount
  def gCount = _gCount
  def opzCount = _opzCount
  def pCount = _pCount
  def pfCount = _pfCount
  def resetCount: this.type = { _fCount = 0; _gCount = 0; _opzCount = 0; _pCount = 0; _pfCount = 0; this }

  class Secret {
    def f = f0.fn
    def g = g0.fn
    def op = op0.ofn
    def p = p0.fn
    def pf = pf0.pfn
    def z = z0.get
  }
  /** Secretly access the functions and transformations (usage not recorded, so will not cause variants to be run) */
  val secret = new Secret

  override def equals(that: Any) = that match {
    case a: Active[_, _] => (f0 == a.f0) && (g0 == a.g0) && (op0 == a.op0) && (p0 == a.p0) && (pf0 == a.pf0)
  }

  override def hashCode = {
    import scala.util.hashing.MurmurHash3._
    import java.lang.System.{identityHashCode => h}
    finalizeHash(mixLast(mix(mix(mix(h(f0), h(g0)), h(op0)), h(p0)), h(pf0)), 5)
  }

  override def toString = f"Active 0x$hashCode%08X"
}
object Active {
  def apply[A, B](f: A ===> A, g: A ===> B, op: OpFn[A], p: A ===> Boolean, pf: ParFn[A]) =
    new Active(f, g, op, p, pf)
}


/** The collection to be tested: this provides all elements and collections
  * and mappings thereon which are available inside the tests.
  */
abstract class Coll[A, B, CC](
  val values: Values,
  val coll: Provider[A, CC],
  val act: Active[A, B]
)(implicit file: sourcecode.File, line: sourcecode.Line) 
extends Sourced {
  /** Arbitrary element of the type in the collection */
  def a: A = coll.silent.a

  /** An arbitrary element of a type not in the basic collection.  Should match g(a). */
  lazy val b: B = act.secret.g(coll.secret.a)

  /** Some function that preserves the type of the elements */
  def f: A => A = act.f

  /** Some function that changes the type of the elements.  Note that g(a) == b should be true. */
  def g: A => B = act.g

  /** Some integer.  May be positive or negative.  Could really be anything. */
  def L: Int = values.L

  /** A fixed positive number that is no bigger than the length of `y` */
  def m: Int = if (values.m < 0) (-values.m-1) % ysize else values.m % ysize

  /** A fixed positive number that may be bigger than the length of `y` **/
  val mm: Int = if (values.mm < 0) -values.mm-1 else values.mm

  /** A fixed positive number that is no bigger than the length of `x` */
  def n: Int = if (values.n < 0) (-values.n-1) % xsize else values.n % xsize

  /** A fixed positive number that may be bigger than the length of `x` */
  def nn: Int = if (values.nn < 0) -values.nn-1 else values.nn

  /** A binary operation on the collection elements.
    *
    * If no such operation exists, leave it as `Coll.noOp` and tests requiring this operator will be skipped.
    */
  def op: (A, A) => A = act.op

  /** A predicate on the type of the elements */
  def p: A => Boolean = act.p

  /** A partial function that preserves the type of the elements */
  def pf: PartialFunction[A, A] = act.pf

  /** A specific collection of some size */
  def x: CC = coll.x

  /** The known size of the collection `x` */
  def xsize: Int = coll.xsize

  /** Another specific collection, not necessarily the same as `x` */
  def y: CC = coll.y

  /** The known size of collection `y` */
  def ysize: Int = coll.ysize

  /** Element of type `A` that is a zero with respect to `op`, if `op` exists and a zero exists */
  def zero: A = act.z

  /** Tests whether this Coll has a zero defined */
  def hasZero = act.hasZ

  /** Another coll (or other code source), if any exists, that was used to help generate this one */
  def origin: Option[Sourced] = None

  /** The location (file and line) where this collection was defined, as a `String` */
  def localize: String = Source.implicitly

  /** This is the actual test which each distinct law must fill in */
  def run: Boolean
}
