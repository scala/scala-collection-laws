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
  val simplest: Numbers = new Numbers(0, 0, 0, 0, 0)
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
