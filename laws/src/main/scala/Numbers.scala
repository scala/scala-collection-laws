package laws

/** Sets of integer values used to check individual cases in certain tests.
  *
  * In general, we expect but do not enforce here that `n` and `m` are positive
  * and bounded by the size of collections `x` and `y` (in the `Coll` in which
  * these `Values` are used), and that `nn` and `mm` are both positive.  `r` can
  * be anything.
  *
  * Do _not_ set `used` yourself!  Just pass it in to `Explore`
  */
final class Numbers(n0: Int, nn0: Int, m0: Int, mm0: Int, r0: Int) {
  val values = Numbers.Values(n0, nn0, m0, mm0, r0)

  val used = Array(false, false, false, false, false)

  def n: Int  = { used(0) = true; values.n }
  def nn: Int = { used(1) = true; values.nn }
  def m: Int  = { used(2) = true; values.m }
  def mm: Int = { used(3) = true; values.mm }
  def r: Int  = { used(4) = true; values.r }

  def setUnused(): this.type = { java.util.Arrays.fill(used, false); this }

  def touched = used(0) || used(1) || used(2) || used(3) || used(4)

  override def equals(that: Any) = that match {
    case nm: Numbers => values == nm.values
    case _           => false
  }

  override def hashCode = values.hashCode

  override def toString = f"Numbers: ${values.n}, ${values.nn}, ${values.m}, ${values.mm}, ${values.r}"
}
object Numbers {
  final case class Values(n: Int, nn: Int, m: Int, mm: Int, r: Int) {}

  val simplest: Numbers = new Numbers(0, 0, 0, 0, 0)

  val possible_n = Array(0, 1, 2, 3, 8, 9, 15, 17, 30, 31, 32, 47, 49, 50, 132, 5100, 5102)
  val possible_nn = possible_n
  val possible_m = possible_n
  val possible_mm = possible_n
  val possible_r = Array(0, 1, 2, 5, 7, 8, 9, 15, 16, 17, 31, 59, 132, 5101, 91347, -1, -24, -1001)

  def apply(n: Int, nn: Int, m: Int, mm: Int, r: Int): Numbers = new Numbers(n, nn, m, mm, r)

  class Restricted(xsize: Int, ysize: Int)
  extends Exploratory[Numbers] {
    val actual_n = possible_n.takeWhile(_ < xsize) match { case ns if ns.length == 0 => Array(-1); case ns => ns }
    val actual_m = possible_m.takeWhile(_ < ysize) match { case ms if ms.length == 0 => Array(-1); case ms => ms }

    val sizes = Array(actual_n.length, possible_nn.length, actual_m.length, possible_mm.length, possible_r.length)

    def lookup(ixs: Array[Int]): Option[Numbers] =
      if (!validate(ixs)) None
      else Some(Numbers(actual_n(ixs(0)), possible_nn(ixs(1)), actual_m(ixs(2)), possible_mm(ixs(3)), possible_r(ixs(4))))
  }
}
