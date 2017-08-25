package laws

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
object Instance { outer =>
  def apply[A, CC](a: A)(x: => CC, xsize: Int)(y: => CC, ysize: Int): Instance[A, CC] =
    new Instance(a, () => x, xsize, () => y, ysize)
  def from[A, CC](a: A, x: Array[A], y: Array[A])(ccf: Array[A] => CC): Instance[A, CC] =
    new Instance(a, () => ccf(x), x.size, () => ccf(y), y.size)
  def cacheFrom[A, CC](a: A, x: Array[A], y: Array[A])(ccf: Array[A] => CC): Instance[A, CC] =
    new Instance(a, new CachedFn0(() => ccf(x)), x.size, new CachedFn0(() => ccf(y)), y.size)

  trait FromArray[A, CC] extends ((A, Array[A], Array[A]) => Instance[A, CC]) with Named {
    def name: String
  }

  def generator[A, CC](ccf: Array[A] => CC)(implicit nm: sourcecode.Name): FromArray[A, CC] = new FromArray[A, CC] {
    def apply(a: A, x: Array[A], y: Array[A]) = from(a, x, y)(ccf)
    def name = nm.value
  }
  def generatorCached[A, CC](ccf: Array[A] => CC)(implicit nm: sourcecode.Name): FromArray[A, CC] = new FromArray[A, CC] {
    def apply(a: A, x: Array[A], y: Array[A]) = cacheFrom(a, x, y)(ccf)
    def name = nm.value
  }

  class Over[A] {
    def generator[CC](ccf: Array[A] => CC)(implicit nm: sourcecode.Name): FromArray[A, CC] = outer.generator(ccf)
    def generatorCached[CC](ccf: Array[A] => CC)(implicit nm: sourcecode.Name): FromArray[A, CC] = outer.generatorCached(ccf)
  }
  def over[A]: Over[A] = new Over[A]
}


/** Provides a source for individual instances we will test.
  * The variable names used are ones we can use to select
  * tests inside the generator.
  */
abstract class InstantiatorsOf[A] {
  private[this] val inst = Instance.over[A]
  val list = inst.generatorCached(_.toList)
  val vector = inst.generatorCached(_.toVector)
  val arrayBuffer = inst.generator(_.to[collection.mutable.ArrayBuffer])
}

object InstantiatorsOfInt extends InstantiatorsOf[Int] {}

object InstantiatorsOfString extends InstantiatorsOf[String] {}
