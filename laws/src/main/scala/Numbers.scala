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
  private[laws] val n0: Int,
  private[laws] val nn0: Int,
  private[laws] val m0: Int,
  private[laws] val mm0: Int,
  private[laws] val r0: Int
) {
  private[this] var _nCount: Int = 0
  private[this] var _nnCount: Int = 0
  private[this] var _mCount: Int = 0
  private[this] var _mmCount: Int = 0
  private[this] var _rCount: Int = 0

  def n: Int = { _nCount += 1; n0 }
  def nn: Int = { _nnCount += 1; nn0 }
  def m: Int = { _mCount += 1; m0 }
  def mm: Int = { _mmCount += 1; mm0 }
  def r: Int = { _rCount += 1; r0 }

  def nCount: Int = _nCount
  def nnCount: Int = _nnCount
  def mCount: Int = _mCount
  def mmCount: Int = _mmCount
  def rCount: Int = _rCount
  def count = Numbers.Count(nCount, nnCount, mCount, mmCount, rCount)
  def resetCount: this.type = { _nCount = 0; _nnCount = 0; _mCount = 0; _mmCount = 0; _rCount = 0; this }

  class Secret {
    def n = n0
    def nn = nn0
    def m = m0
    def mm = mm0
    def r = r0
  }
  /** Allow the values to be accessed without recording them (so as not to trigger tests of variants in the parameter) */
  val secret = new Secret

  override def equals(that: Any) = that match {
    case v: Numbers => n0 == v.n0 && nn0 == v.nn0 && m0 == v.m0 && mm0 == v.mm0 && r0 == v.r0
    case _          => false
  }
  override def hashCode = {
    import scala.util.hashing.MurmurHash3._
    finalizeHash(mixLast(mix(mix(mix(n0, nn0), m0), mm0), r0), 5)
  }
  override def toString = f"Numbers: $n0, $nn0, $m0, $mm0, $r0"
}
object Numbers {
  case class Count(nCount: Int, nnCount: Int, mCount: Int, mmCount: Int, rCount: Int) {
    def -(that: Count) = new Count(
      nCount  - that.nCount, 
      nnCount - that.nnCount,
      mCount  - that.mCount,
      mmCount - that.mmCount,
      rCount  - that.rCount
    )
    def isnt(that: Count) = Array(
      nCount != that.nCount,
      nnCount != that.nnCount,
      mCount != that.mCount,
      mmCount != that.mmCount,
      rCount != that.rCount
    )
  }
  val simplest: Numbers = new Numbers(0, 0, 0, 0, 0)
  def apply(n: Int, nn: Int, m: Int, mm: Int, r: Int): Numbers = new Numbers(n, nn, m, mm, r)

  val possible_r = Array(0, 1, 2, 5, 7, 8, 9, 15, 16, 17, 31, 59, 132, 5101, 91347, -1, -24, -1001)
  val possible_m = Array(0, 1, 2, 3, 8, 9, 15, 17, 30, 31, 32, 47, 49, 50, 132, 5100, 5102)
  val possible_mm = possible_m
  val possible_n = possible_m
  val possible_nn = possible_m

  class Restricted(val xsize: Int, val ysize: Int)
  extends Exploratory[Numbers] {
    val actual_n = possible_n.takeWhile(_ < xsize) match { case ns if ns.length == 0 => Array(-1); case ns => ns }
    val actual_m = possible_m.takeWhile(_ < ysize) match { case ms if ms.length == 0 => Array(-1); case ms => ms }

    val sizes = Array(actual_n.length, possible_nn.length, actual_m.length, possible_mm.length, possible_r.length)

    def lookup(ixs: Array[Int]): Option[Numbers] =
      if (!validate(ixs)) None
      else Some(Numbers(actual_n(ixs(0)), possible_nn(ixs(1)), actual_m(ixs(2)), possible_mm(ixs(3)), possible_r(ixs(4))))
  }
}
