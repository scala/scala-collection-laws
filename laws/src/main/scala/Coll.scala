package laws

/** Trait that captures the idea of having a source from which one is generated.
  */
trait Sourced {
  final def source(implicit file: sourcecode.File, line: sourcecode.Line) = Sourced.local(file, line)
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
class Numbers private (
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
    case v: Numbers => L0 == v.L0 && m0 == v.m0 && mm0 == v.mm0 && n0 == v.n0 && nn0 == v.nn0
    case _          => false
  }
  override def hashCode = {
    import scala.util.hashing.MurmurHash3._
    finalizeHash(mixLast(mix(mix(mix(m0, L0), mm0), n0), nn0), 5)
  }
  override def toString = f"Numbers: $L0, $m0, $mm0, $n0, $nn0"
}
object Numbers {
  def apply(L: Int, m: Int, mm: Int, n: Int, nn: Int): Numbers = new Numbers(L, m, mm, n, nn)
}

/** A provider of instances of collections of a particular type with a particular element type.
  * Each call to the methods should return the same collection; if mutable, the instance
  * should be created afresh each time.  If immutable, it shouldn't matter.
  * (Laws that check reference identity should cache the value.)
  *
  * That `A` is actually an element that can be found within `CC` is not enforced.
  */
class Instance[A, CC] private (
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
    def x: CC = x0()
    def xsize: Int = xsize0
    def y: CC = y0()
    def ysize: Int = ysize0
  }
  /** Secretly access the input collections (usage not recorded, so will not cause variants to be run) */
  val secret = new Secret

  override def equals(that: Any) = that match {
    case i: Instance[_, _] => (this eq i) || (a0 == i.a0 && xsize0 == i.xsize0 && ysize0 == i.ysize0 && x0() == i.x0() && y0() == i.y0)
    case _                 => false
  }
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3._
    finalizeHash(mixLast(mix(x0().##, y0().##), a0.##), 1 + xsize0 + ysize0)
  }
  override lazy val toString = {
    def clip(s: String, n: Int) = if (s.length <= n) s else s.substring(0, n-3)+"..."
    f"Provider: singleton $a0 with\n  ${clip(x0().toString, 61)} ; len $xsize0\n  ${clip(y0().toString, 61)} ; len $ysize0"
  }
}
object Instance {
  def apply[A, CC](a: A)(x: => CC, xsize: Int)(y: => CC, ysize: Int): Instance[A, CC] =
    new Instance(a, () => x, xsize, () => y, ysize)
  def from[A, CC](a: A, x: Array[A], y: Array[A])(cc: Array[A] => CC): Instance[A, CC] =
    new Instance(a, () => cc(x), x.size, () => cc(y), y.size)
}


/** The collection to be tested: this provides all elements and collections
  * and mappings thereon which are available inside the tests.
  */
abstract class Test[A, B, CC](
  val num: Numbers,
  val instance: Instance[A, CC],
  val act: Active[A, B]
)(implicit file: sourcecode.File, line: sourcecode.Line, name: sourcecode.Name) 
extends Sourced {
  /** Arbitrary element of the type in the collection */
  def a: A = instance.a

  /** An arbitrary element of a type not in the basic collection.  Should match g(a). */
  lazy val b: B = act.secret.g(instance.secret.a)

  /** Some function that preserves the type of the elements */
  def f: A => A = act.f

  /** Some function that changes the type of the elements.  Note that g(a) == b should be true. */
  def g: A => B = act.g

  /** Some integer.  May be positive or negative.  Could really be anything. */
  def L: Int = num.L

  /** A fixed positive number that is no bigger than the length of `y` */
  def m: Int = if (num.secret.m < 0) (-num.m-1) % ysize else num.m % ysize

  /** A fixed positive number that may be bigger than the length of `y` **/
  val mm: Int = if (num.secret.mm < 0) -num.mm-1 else num.mm

  /** A fixed positive number that is no bigger than the length of `x` */
  def n: Int = if (num.secret.n < 0) (-num.n-1) % xsize else num.n % xsize

  /** A fixed positive number that may be bigger than the length of `x` */
  def nn: Int = if (num.secret.nn < 0) -num.nn-1 else num.nn

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
  def x: CC = instance.x

  /** The known size of the collection `x` */
  def xsize: Int = instance.xsize

  /** Another specific collection, not necessarily the same as `x` */
  def y: CC = instance.y

  /** The known size of collection `y` */
  def ysize: Int = instance.ysize

  /** Element of type `A` that is a zero with respect to `op`, if `op` exists and a zero exists */
  def zero: A = act.z

  /** Tests whether this Coll has a zero defined */
  def hasZero = act.hasZ

  override lazy val toString =
    name.value.toString + " @ " + source +
    f"\n  $num\n" +
    instance.toString.split("\n").map("  " + _).mkString("", "\n", "\n") +
    act.toString.split("\n").map("  " + _).mkString("", "\n", "\n")

  /** This is the actual test which each distinct law must fill in */
  def run: Boolean
}


///////////////////////////////////////
// Selection of base collection type //
///////////////////////////////////////

abstract class IntTest[CC](
  num: Numbers, instance: Instance[Int, CC], act: Active[Int, Long]
)(
  implicit file: sourcecode.File, line: sourcecode.Line, name: sourcecode.Name
)
extends Test[Int, Long, CC](num, instance, act)(file, line, name) {}

abstract class StrTest[CC](
  num: Numbers, instance: Instance[String, CC], act: Active[String, Option[String]]
)(
  implicit file: sourcecode.File, line: sourcecode.Line, name: sourcecode.Name
)
extends Test[String, Option[String], CC](num, instance, act)(file, line, name) {}


