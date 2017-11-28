package laws

import scala.reflect.runtime.universe.TypeTag

/** A provider of instances of collections of a particular type with a particular element type.
  * Each call to the methods should return the same collection; if mutable, the instance
  * should be created afresh each time.  If immutable, it shouldn't matter.
  * (Laws that check reference identity should cache the value.)
  *
  * That `A` is actually an element that can be found within `CC` is not enforced.
  *
  * Do not directly alter `used`; just pass it to the appropriate `Explore`.
  */
class Instance[A, CC: TypeTag] protected (
  a0: A, x0: () => CC, xsize0: Int, y0: () => CC, ysize0: Int,
  val flags: Set[Flag], implicitMethods: MethodChecker = MethodChecker.empty
) {
  val values = Instance.Values(a0, x0, xsize0, y0, ysize0)

  val used = Array(false, false, false)

  /** An example element of a type that can be found within the collection */
  def a: A = { used(0) = true; a0 }

  /** A particular instance of a collection. */
  def x: CC = { used(1) = true; x0() }

  /** The size of the collection `x` */
  def xsize: Int = { used(1) = true; xsize0 }

  /** Another instance of a collection, which may or may not be the same as `x` */
  def y: CC = { used(2) = true; y0() }

  /** The size of the collection `y` */
  def ysize: Int = { used(2) = true; ysize0 }

  /** The methods available on this collection */
  lazy val methods = MethodChecker.from[CC] | implicitMethods

  /** Allows you to introduce more methods, e.g. those enriched via an implicit class */
  def moreMethods(mc: MethodChecker): Instance[A, CC] = new Instance[A, CC](a0, x0, xsize, y0, ysize, flags, methods | mc)

  def setUnused(): this.type = { java.util.Arrays.fill(used, false); this }

  def touched = used(0) || used(1) || used(2)

  /** Override this method to provide a more useful description in case of error
    * if the default `toString` is not helpful.
    */
  protected def stringify(cc: CC): String = cc match {
    case i: Iterator[_] => i.mkString("Iterator(", ", ", ")")
    case a: Array[_]    => a.mkString("Array(", ", ", ")")
    case _              => cc.toString()
  }

  override def equals(that: Any) = that match {
    case i: Instance[_, _] => (this eq i) || (this.values == i.values)
    case _                 => false
  }
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3._
    finalizeHash(mixLast(mix(x0().##, y0().##), a0.##), 1 + xsize0 + ysize0)
  }
  override lazy val toString = {
    def clip(s: String, n: Int) = if (s.length <= n) s else s.substring(0, n-3)+"..."
    f"Provider: singleton $a0 with\n  ${clip(stringify(x0()), 61)} ; len $xsize0\n  ${clip(stringify(y0()), 61)} ; len $ysize0"
  }
}
object Instance { outer =>
  /** Provides a single element and the appropriate collections, without logging of usage.
    *
    * Note that `x` and `y` are generators for collections.
    */
  final case class Values[A, CC: TypeTag](a: A, x: () => CC, xsize: Int, y: () => CC, ysize: Int)

  /** Information on how to refer to the instance. */
  trait PackagePath {
    def nickname: String
    def fullyQualified: String
  }

  /** Generates an instance from collection generators and flags. */
  def apply[A, CC: TypeTag](a: A)(x: => CC, xsize: Int)(y: => CC, ysize: Int)(flags: Set[Flag] = Set.empty): Instance[A, CC] =
    new Instance(
      a,
      () => x, xsize,
      () => y, ysize,
      flags
    )

  /** Generates an instance from arrays and a transformation function, regenerating the collection each time. */
  def from[A, CC: TypeTag: Sizable](a: A, x: Array[A], y: Array[A])(ccf: Array[A] => CC)(flags: Set[Flag] = Set.empty): Instance[A, CC] =
    new Instance(
      a,
      () => ccf(x), implicitly[Sizable[CC]].sizeof(ccf(x)),
      () => ccf(y), implicitly[Sizable[CC]].sizeof(ccf(y)),
      flags
    )

  /** Generates an instance from arrays and a transformation function, caching the generated collection. */
  def cacheFrom[A, CC: TypeTag: Sizable](a: A, x: Array[A], y: Array[A])(ccf: Array[A] => CC)(flags: Set[Flag] = Set.empty): Instance[A, CC] =
    new Instance(
      a,
      new CachedFn0(() => ccf(x)), implicitly[Sizable[CC]].sizeof(ccf(x)),
      new CachedFn0(() => ccf(y)), implicitly[Sizable[CC]].sizeof(ccf(y)), 
      flags
    )

  /** Encapsulates the capability of converting elements and arrays of elements into an `Instance` for a collection/element type */
  trait FromArray[A, CC] extends ((A, Array[A], Array[A]) => Instance[A, CC]) with Named { self =>
    def moreMethods(mc: MethodChecker): FromArray[A, CC] = new FromArray[A, CC] {
      def name = self.name
      def apply(a: A, x: Array[A], y: Array[A]) = self.apply(a, x, y).moreMethods(mc)
    }
    override def toString = f"$name from array"
  }

  /** Produce a `FromArray` generator for instances given an array-to-collection transformation method. */
  def makeWith[A, CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Flag*)(implicit nm: sourcecode.Name): FromArray[A, CC] =
    new FromArray[A, CC] {
      def apply(a: A, x: Array[A], y: Array[A]) = from(a, x, y)(ccf)(flags.toSet)
      def name = nm.value
    }

  /** Produce a `FromArray` generator with caching for instances given an array-to-collection transformation method. */
  def cacheWith[A, CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Flag*)(implicit nm: sourcecode.Name): FromArray[A, CC] =
    new FromArray[A, CC] {
      def apply(a: A, x: Array[A], y: Array[A]) = cacheFrom(a, x, y)(ccf)(flags.toSet)
      def name = nm.value
    }

  /** Adds default flags to collection generators */
  class Flagged[A](allFlags: Flag*) {
    def makeWith[CC](ccf: Array[A] => CC, flags: Flag*)(implicit nm: sourcecode.Name, tt: TypeTag[CC], sz: Sizable[CC]): FromArray[A, CC] =
      outer.makeWith(ccf, (allFlags ++ flags): _*)
    def cacheWith[CC](ccf: Array[A] => CC, flags: Flag*)(implicit nm: sourcecode.Name, tt: TypeTag[CC], sz: Sizable[CC]): FromArray[A, CC] =
      outer.cacheWith(ccf, (allFlags ++ flags): _*)
  }
  def flagged[A](allFlags: Flag*): Flagged[A] = new Flagged[A](allFlags: _*)

  /** Keeps track of whether instances actually get prepared for deployment in tests.
    *
    * This is necessary because tests have to _manually_ refer to each collection type, in part to
    * allow sufficient customization.
    */
  trait Deployed {
    def accesses: Int
    def name: String
    def group: String
    def path = group + ": " + name
  }
}
