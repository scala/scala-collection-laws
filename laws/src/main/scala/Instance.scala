package laws

import scala.language.higherKinds

import scala.reflect.runtime.universe.TypeTag

trait Containing[A] {
  def methods: Set[String]
  def example: A
  def majorSize: Int
}

/** A provider of instances of collections of a particular type with a particular element type.
  * Each call to the methods should return the same collection; if mutable, the instance
  * should be created afresh each time.  If immutable, it shouldn't matter.
  * (Laws that check reference identity should cache the value.)
  *
  * That `A` is actually an element that can be found within `CC` is not enforced.
  */
class Instance[A, CC: TypeTag] private (
  private[laws] val x0: () => CC,
  private[laws] val xsize0: Int,
  private[laws] val y0: () => CC,
  private[laws] val ysize0: Int,
  private[laws] val a0: A,
  val flags: Set[Tag]
)
extends Containing[A] {
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

  lazy val methods = MethodChecker.list(x)

  def aCount: Int = _aCount
  def xCount: Int = _xCount
  def xsizeCount: Int = _xsizeCount
  def yCount: Int = _yCount
  def ysizeCount: Int = _ysizeCount
  def count = Instance.Count(aCount, xCount, xsizeCount, yCount, ysizeCount)
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

  def example = secret.a
  def majorSize = secret.xsize

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
  case class Count(aCount: Int, xCount: Int, xsizeCount: Int, yCount: Int, ysizeCount: Int) {
    def -(that: Count) = new Count(
      aCount     - that.aCount,
      xCount     - that.xCount,
      xsizeCount - that.xsizeCount,
      yCount     - that.yCount,
      ysizeCount - that.ysizeCount
    )
    def isnt(that: Count) = Array(
      aCount != that.aCount,
      xCount != that.xCount || xsizeCount != that.xsizeCount,
      yCount != that.yCount || ysizeCount != that.ysizeCount
    )
  }

  trait Sizable[CC] {
    def sizeof(c: CC): Int
  }

  trait PackagePath {
    def nickname: String
    def fullyQualified: String
  }

  def apply[A, CC: TypeTag: Sizable](a: A)(x: => CC, xsize: Int)(y: => CC, ysize: Int)(flags: Set[Tag] = Set.empty): Instance[A, CC] =
    new Instance(
      () => x, xsize,
      () => y, ysize,
      a, flags
    )
  def from[A, CC: TypeTag: Sizable](a: A, x: Array[A], y: Array[A])(ccf: Array[A] => CC)(flags: Set[Tag] = Set.empty): Instance[A, CC] =
    new Instance( 
      () => ccf(x), implicitly[Sizable[CC]].sizeof(ccf(x)),
      () => ccf(y), implicitly[Sizable[CC]].sizeof(ccf(y)),
      a, flags
    )
  def cacheFrom[A, CC: TypeTag: Sizable](a: A, x: Array[A], y: Array[A])(ccf: Array[A] => CC)(flags: Set[Tag] = Set.empty): Instance[A, CC] =
    new Instance(
      new CachedFn0(() => ccf(x)), implicitly[Sizable[CC]].sizeof(ccf(x)),
      new CachedFn0(() => ccf(y)), implicitly[Sizable[CC]].sizeof(ccf(y)), 
      a, flags
    )

  trait ContainingFromArray[A] extends ((A, Array[A], Array[A]) => Containing[A]) with Named {
    override def toString = f"$name from array"
  }
  trait FromArray[A, CC] extends ((A, Array[A], Array[A]) => Instance[A, CC]) with ContainingFromArray[A] {}

  def generator[A, CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Tag*)(implicit nm: sourcecode.Name): FromArray[A, CC] =
    new FromArray[A, CC] {
      def apply(a: A, x: Array[A], y: Array[A]) = from(a, x, y)(ccf)(flags.toSet)
      def name = nm.value
    }
  def generatorCached[A, CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Tag*)(implicit nm: sourcecode.Name): FromArray[A, CC] =
    new FromArray[A, CC] {
      def apply(a: A, x: Array[A], y: Array[A]) = cacheFrom(a, x, y)(ccf)(flags.toSet)
      def name = nm.value
    }

  class Over[A](allFlags: Tag*) {
    def generator[CC](ccf: Array[A] => CC, flags: Tag*)(implicit nm: sourcecode.Name, tt: TypeTag[CC], sz: Sizable[CC]): FromArray[A, CC] =
      outer.generator(ccf, (allFlags ++ flags): _*)
    def generatorCached[CC](ccf: Array[A] => CC, flags: Tag*)(implicit nm: sourcecode.Name, tt: TypeTag[CC], sz: Sizable[CC]): FromArray[A, CC] =
      outer.generatorCached(ccf, (allFlags ++ flags): _*)
  }
  def over[A](allFlags: Tag*): Over[A] = new Over[A](allFlags: _*)
}


/** Provides a source for individual instances we will test.
  * The variable names used are ones we can use to select
  * tests inside the generator.
  */
abstract class InstantiatorsOf[A]
extends Exploratory[(A, Array[A], Array[A])] {
  import Instance.Sizable
  import Tag._

  protected implicit def orderingOfA: Ordering[A]
  protected implicit def typeTagA: TypeTag[A]
  protected def allFlags: Array[Tag]
  private[this] val inst = Instance.over[A](allFlags: _*)

  protected implicit def sizeOfSeq[A, S[A] <: collection.Seq[A]] = new Sizable[S[A]] { def sizeof(s: S[A]) = s.length }
  protected implicit def sizeOfOnce[A, O[A] <: collection.Traversable[A]] = new Sizable[O[A]] { def sizeof(o: O[A]) = o.size }
  protected implicit def sizeOfArray[A] = new Sizable[Array[A]] { def sizeof(a: Array[A]) = a.length }
  protected implicit val sizeOfString = new Sizable[String] { def sizeof(s: String) = s.length }

  protected val registry = Vector.newBuilder[Instance.ContainingFromArray[A]]

  object Imm extends Instance.PackagePath {
    def nickname = "Imm"
    def fullyQualified = "scala.collection.immutable"
    def C[CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Tag*)(implicit nm: sourcecode.Name) = {
      val ans = inst.generatorCached(ccf, flags: _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      registry += ans
      ans
    }
    val hashSet     = C(_.to[collection.immutable.HashSet], SET)
    val list        = C(_.toList, SEQ)
    val set         = C(_.toSet, SET)
    val sortedSet   = C(_.to[collection.immutable.SortedSet], SET)
    val stream      = C(_.to[Stream], SEQ)
    val vector      = C(_.toVector, SEQ)
  }

  object Mut extends Instance.PackagePath {
    def nickname = "Mut"
    def fullyQualified = "scala.collection.mutable"
    def C[CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Tag*)(implicit nm: sourcecode.Name) = {
      val ans = inst.generator(ccf, flags: _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      registry += ans
      ans
    }
    val arrayBuffer  = C(_.to[collection.mutable.ArrayBuffer], SEQ)
    val array        = C(_.clone, SEQ, ARR)
    val hashSet      = C(_.to[collection.mutable.HashSet], SET)
    val wrappedArray = C(_.clone: collection.mutable.WrappedArray[A], SEQ)
  }

  def possible_a: Array[A]
  def possible_x: Array[Array[A]]
  def possible_y: Array[Array[A]]

  val sizes = Array(possible_a.length, possible_x.length, possible_y.length)
  
  def lookup(ixs: Array[Int]): Option[(A, Array[A], Array[A])] =
    if (!validate(ixs)) None
    else Some((possible_a(ixs(0)), possible_x(ixs(1)), possible_y(ixs(2))))

  lazy val all = registry.result
}

trait InstantiatorsOfKV[K, V] extends Exploratory[((K, V), Array[(K, V)], Array[(K, V)])] { self: InstantiatorsOf[(K, V)] =>
  import Instance.Sizable
  import Tag._

  protected implicit def orderingOfK: Ordering[K]

  protected implicit def typeTagK: TypeTag[K]
  protected implicit def typeTagV: TypeTag[V]
  protected def kvInst = Instance.over[(K, V)](allFlags: _*)
  protected implicit def sizeOfMap[K, V, M[K, V] <: collection.Map[K, V]] = new Sizable[M[K, V]] { def sizeof(m: M[K, V]) = m.size }

  object ImmKV extends Instance.PackagePath {
    def nickname = "ImmKV"
    def fullyQualified = "scala.collection.immutable"
    def C[CC: TypeTag: Sizable](ccf: Array[(K, V)] => CC, flags: Tag*)(implicit nm: sourcecode.Name) = {
      val ans = kvInst.generatorCached(ccf, (MAP +: flags): _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      registry += ans
      ans
    }
    val hashMap = C({ a => val mb = collection.immutable.HashMap.newBuilder[K, V]; for (kv <- a) mb += kv; mb.result })
    val listMap = C({ a => val mb = collection.immutable.HashMap.newBuilder[K, V]; for (kv <- a) mb += kv; mb.result })
    val sortedMap = C({ a => val mb = collection.immutable.SortedMap.newBuilder[K, V]; for (kv <- a) mb += kv; mb.result })
    val treeMap = C({ a => val mb = collection.immutable.TreeMap.newBuilder[K, V]; for (kv <- a) mb += kv; mb.result })
  }

  object MutKV extends Instance.PackagePath {
    def nickname = "MutKV"
    def fullyQualified = "scala.collection.mutable"
    def C[CC: TypeTag: Sizable](ccf: Array[(K, V)] => CC, flags: Tag*)(implicit nm: sourcecode.Name) = {
      val ans = kvInst.generator(ccf, (MAP +: flags): _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      registry += ans
      ans
    }
    val hashMap =       C({ a => val m = new collection.mutable.HashMap[K, V];       for (kv <- a) m += kv; m })
    val listMap =       C({ a => val m = new collection.mutable.ListMap[K, V];       for (kv <- a) m += kv; m })
    val linkedHashMap = C({ a => val m = new collection.mutable.LinkedHashMap[K, V]; for (kv <- a) m += kv; m })
    val openHashMap =   C({ a => val m = new collection.mutable.OpenHashMap[K, V];   for (kv <- a) m += kv; m })
    val sortedMap =     C({ a => val m = collection.mutable.SortedMap.empty[K, V];   for (kv <- a) m += kv; m })
    val treeMap =       C({ a => val m = new collection.mutable.TreeMap[K, V];       for (kv <- a) m += kv; m })
  }
}

object OrderingSource {
  val orderingOfLong = implicitly[Ordering[Long]]
  val orderingOfInt = implicitly[Ordering[Int]]
  val orderingOfString = implicitly[Ordering[String]]
  val orderingOfLongString = implicitly[Ordering[(Long, String)]]
  val orderingOfStringLong = implicitly[Ordering[(String, Long)]]
}

object TypeTagSource {
  val typeTagInt = implicitly[TypeTag[Int]]
  val typeTagLong = implicitly[TypeTag[Long]]
  val typeTagString = implicitly[TypeTag[String]]
  val typeTagLongString = implicitly[TypeTag[(Long, String)]]
  val typeTagStringLong = implicitly[TypeTag[(String, Long)]]
}

object InstantiatorsOfInt extends InstantiatorsOf[Int] {
  import Tag._

  protected implicit def orderingOfA = OrderingSource.orderingOfInt
  protected implicit def typeTagA = TypeTagSource.typeTagInt
  protected def allFlags = Array(INT)

  lazy val possible_a = Array(0, 1, 2, 3, 4, 5, 7, 8, 9, 15, 16, 17, 23, 31, 47, 152, 3133, 1294814, -1, -2, -6, -19, -1915, -19298157)
  lazy val possible_x = Array(
    Array.empty[Int],
    Array(0),
    Array(125),
    Array(-15),
    Array(0, 1),
    Array(0, 1, 2),
    Array(0, 1, 2, 3),
    Array(0, 1, 2, 3, 4),
    Array(4, 4, 4, 4, 4),
    Array(0, 1, 2, 3, 4, 5, 6),
    Array(0, 1, 2, 3, 4, 5, 6, 7),
    Array(0, 1, 2, 3, 4, 5, 6, 7, 8),
    Array(10, 10, 10, 10, 10, 5, 5, 5, 5, 1, 1, 1),
    Array(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    Array.range(0,31),
    Array.range(0,32),
    Array.range(0,33),
    Array.range(0,192),
    Array.fill(1025)(42),
    Array.range(0,8111)
  )
  lazy val possible_y = possible_x
}

object InstantiatorsOfStr extends InstantiatorsOf[String] {
  import Tag._

  protected implicit def orderingOfA = OrderingSource.orderingOfString
  protected implicit def typeTagA = TypeTagSource.typeTagString
  protected def allFlags = Array(STR)

  lazy val possible_a = Array(
    "", "0", "one", "salmon", "\u0000\u0000\u0000\u0000", "the quick brown fox jumps over the lazy dog", "\u1517\u1851..!"
  )
  lazy val possible_x = Array(
    Array.empty[String],
    Array(possible_a(1)),
    Array(possible_a(3)),
    possible_a,
    Array("0", "1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1"),
    Array.range(-44, 45).map(_.toString),
    Array.fill(184)("herring")
  )
  lazy val possible_y = possible_x
}

object InstantiatorsOfLongStr extends InstantiatorsOf[(Long, String)] with InstantiatorsOfKV[Long, String] {
  import Tag._

  protected implicit def orderingOfA = OrderingSource.orderingOfLongString
  protected implicit def orderingOfK = OrderingSource.orderingOfLong
  protected implicit def typeTagA = TypeTagSource.typeTagLongString
  protected implicit def typeTagK = TypeTagSource.typeTagLong
  protected implicit def typeTagV = TypeTagSource.typeTagString
  protected def allFlags = Array[Tag]()

  lazy val possible_a = Array(3L -> "wish")
  lazy val possible_x = Array(
    Array.empty[(Long, String)],
    possible_a,
    Array(1L -> "herring", 2L -> "cod", 3L -> "salmon")
  )
  lazy val possible_y = possible_x
}

object InstantiatorsOfStrLong extends InstantiatorsOf[(String, Long)] with InstantiatorsOfKV[String, Long] {
  import Tag._

  protected implicit def orderingOfA = OrderingSource.orderingOfStringLong
  protected implicit def orderingOfK = OrderingSource.orderingOfString
  protected implicit def typeTagA = TypeTagSource.typeTagStringLong
  protected implicit def typeTagK = TypeTagSource.typeTagString
  protected implicit def typeTagV = TypeTagSource.typeTagLong
  protected def allFlags = Array[Tag]()

  lazy val possible_a = Array("wish" -> 3L)
  lazy val possible_x = Array(
    Array.empty[(String, Long)],
    possible_a,
    Array("herring" -> 1L, "cod" -> 2L, "salmon" -> 3L)
  )
  lazy val possible_y = possible_x
}