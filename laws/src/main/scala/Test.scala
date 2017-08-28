package laws

/** Sets of integer values used to check individual cases in certain tests.
  *
  * In general, we expect but do not enforce here that `n` and `m` are positive
  * and bounded by the size of collections `x` and `y` (in the `Coll` in which
  * these `Values` are used), and that `nn` and `mm` are both positive.  `L` can
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
  def count = Numbers.Count(LCount, mCount, mmCount, nCount, nnCount)
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
  case class Count(LCount: Int, mCount: Int, mmCount: Int, nCount: Int, nnCount: Int) {
    def -(that: Count) = new Count(
      LCount  - that.LCount,
      mCount  - that.mCount,
      mmCount - that.mmCount,
      nCount  - that.nCount, 
      nnCount - that.nnCount
    )
  }
  def apply(L: Int, m: Int, mm: Int, n: Int, nn: Int): Numbers = new Numbers(L, m, mm, n, nn)

  val possible_L = Array(0, 1, 2, 5, 7, 8, 9, 15, 16, 17, 31, 59, 132, 5101, 91347, -1, -24, -1001)
  val possible_m = Array(0, 1, 2, 3, 8, 9, 15, 17, 30, 31, 32, 47, 49, 50, 132, 5100, 5102)
  val possible_mm = possible_m
  val possible_n = possible_m
  val possible_nn = possible_m

  class Restricted(val xsize: Int, val ysize: Int)
  extends Exploratory[Numbers] {
    val actual_n = possible_n.takeWhile(_ < xsize) match { case ns if ns.length == 0 => Array(-1); case ns => ns }
    val actual_m = possible_m.takeWhile(_ < ysize) match { case ms if ms.length == 0 => Array(-1); case ms => ms }

    val sizes = Array(possible_L.length, actual_m.length, possible_mm.length, actual_n.length, possible_nn.length)

    def lookup(ixs: Array[Int]): Option[Numbers] =
      if (!validate(ixs)) None
      else Some(Numbers(possible_L(ixs(0)), actual_m(ixs(1)), possible_mm(ixs(2)), actual_n(ixs(3)), possible_nn(ixs(4))))
  }
}

/** The collection to be tested: this provides all elements and collections
  * and mappings thereon which are available inside the tests.
  */
abstract class Test[A, B, CC](
  val num: Numbers,
  val instance: Instance[A, CC],
  val ops: Ops[A, B]
)(implicit file: sourcecode.File, line: sourcecode.Line, nm: sourcecode.Name) 
extends Sourced
with Named {
  /** Arbitrary element of the type in the collection */
  def a: A = instance.a

  /** An arbitrary element of a type not in the basic collection.  Should match g(a). */
  lazy val b: B = ops.secret.g(instance.secret.a)

  /** Some function that preserves the type of the elements */
  def f: A => A = ops.f

  /** Some function that changes the type of the elements.  Note that g(a) == b should be true. */
  def g: A => B = ops.g

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
  def op: (A, A) => A = ops.op

  /** A predicate on the type of the elements */
  def p: A => Boolean = ops.p

  /** A partial function that preserves the type of the elements */
  def pf: PartialFunction[A, A] = ops.pf

  /** A specific collection of some size */
  def x: CC = instance.x

  /** The known size of the collection `x` */
  def xsize: Int = instance.xsize

  /** Another specific collection, not necessarily the same as `x` */
  def y: CC = instance.y

  /** The known size of collection `y` */
  def ysize: Int = instance.ysize

  /** Element of type `A` that is a zero with respect to `op`, if `op` exists and a zero exists */
  def zero: A = ops.z

  /** Tests whether this Coll has a zero defined */
  def hasZero = ops.hasZ

  def name = nm.value.toString

  def count = Test.Count(num.count, instance.count , ops.count)

  def resetCount { num.resetCount; instance.resetCount; ops.resetCount }

  override lazy val toString =
    nm.value.toString + " @ " + source +
    f"\n  $num\n" +
    instance.toString.split("\n").map("  " + _).mkString("", "\n", "\n") +
    ops.toString.split("\n").map("  " + _).mkString("", "\n", "\n")

  /** The law tested by this (kind of) test, and the ability to run it */
  def law: Law

  /** This is the actual test which runs a law and returns `true` if it passes with these parameters */
  def run: Boolean
}
object Test {
  case class Count(numbers: Numbers.Count, instances: Instance.Count, ops: Ops.Count) {
    def -(that: Count) = new Count(numbers - that.numbers, instances - that.instances, ops - that.ops)
  }

  private class N(var count: Int = 0) { def ++(){ count += 1 } }
  class ComparesTo[A, CC](me: CC)(implicit onceCC: CC => collection.TraversableOnce[A]) {
    def theSameAs[DD](you: DD)(implicit onceDD: DD => collection.TraversableOnce[A]) = {
      val meB, youB = collection.mutable.ArrayBuffer.empty[A]
      onceCC(me).foreach(meB += _)
      onceDD(you).foreach(youB += _)
      meB == youB
    }
    def hasAllOf[DD](you: DD)(implicit onceDD: DD => collection.TraversableOnce[A]) = {
      val meM, youM = collection.mutable.HashMap.empty[A, N]
      onceCC(me).foreach(a => meM.getOrElseUpdate(a, new N).++)
      onceDD(you).foreach(a => youM.getOrElseUpdate(a, new N).++)
      meM.forall{ case (a, n) => youM.get(a).exists(_.count == n.count) } &&
      youM.forall{ case (a, n) => meM contains a }
    }
    def isPartOf[DD](you: DD)(implicit onceDD: DD => collection.TraversableOnce[A]) = {
      val meM, youM = collection.mutable.HashMap.empty[A, N]
      onceCC(me).foreach(a => meM.getOrElseUpdate(a, new N).++)
      onceDD(you).foreach(a => youM.getOrElseUpdate(a, new N).++)
      meM.forall{ case (a, n) => youM.get(a).exists(_.count >= n.count) }
    }

  }
}


///////////////////////////////////////
// Selection of base collection type //
///////////////////////////////////////

abstract class IntTest[CC](
  num: Numbers, instance: Instance[Int, CC], ops: Ops[Int, Long]
)(
  implicit file: sourcecode.File, line: sourcecode.Line, name: sourcecode.Name
)
extends Test[Int, Long, CC](num, instance, ops)(file, line, name) {
  type A = Int
  type B = Long
}

abstract class StrTest[CC](
  num: Numbers, instance: Instance[String, CC], ops: Ops[String, Option[String]]
)(
  implicit file: sourcecode.File, line: sourcecode.Line, name: sourcecode.Name
)
extends Test[String, Option[String], CC](num, instance, ops)(file, line, name) {
  type A = String
  type B = Option[String]
}
