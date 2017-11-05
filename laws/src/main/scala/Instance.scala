package laws

import scala.language.higherKinds

import scala.reflect.runtime.universe.TypeTag

/** A provider of instances of collections of a particular type with a particular element type.
  * Each call to the methods should return the same collection; if mutable, the instance
  * should be created afresh each time.  If immutable, it shouldn't matter.
  * (Laws that check reference identity should cache the value.)
  *
  * That `A` is actually an element that can be found within `CC` is not enforced.
  */
class Instance[A, CC: TypeTag] protected (a0: A, x0: () => CC, xsize0: Int, y0: () => CC, ysize0: Int, val flags: Set[Tag]) {
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

  lazy val methods = MethodChecker.list(x)

  def setUnused(): this.type = { java.util.Arrays.fill(used, false); this }

  def touched = used(0) || used(1) || used(2)

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
    f"Provider: singleton $a0 with\n  ${clip(x0().toString, 61)} ; len $xsize0\n  ${clip(y0().toString, 61)} ; len $ysize0"
  }
}
object Instance { outer =>
  final case class Values[A, CC: TypeTag](a: A, x: () => CC, xsize: Int, y: () => CC, ysize: Int)

  trait Sizable[CC] {
    def sizeof(c: CC): Int
  }

  trait PackagePath {
    def nickname: String
    def fullyQualified: String
  }

  def apply[A, CC: TypeTag](a: A)(x: => CC, xsize: Int)(y: => CC, ysize: Int)(flags: Set[Tag] = Set.empty): Instance[A, CC] =
    new Instance(
      a,
      () => x, xsize,
      () => y, ysize,
      flags
    )
  def from[A, CC: TypeTag: Sizable](a: A, x: Array[A], y: Array[A])(ccf: Array[A] => CC)(flags: Set[Tag] = Set.empty): Instance[A, CC] =
    new Instance(
      a,
      () => ccf(x), implicitly[Sizable[CC]].sizeof(ccf(x)),
      () => ccf(y), implicitly[Sizable[CC]].sizeof(ccf(y)),
      flags
    )
  def cacheFrom[A, CC: TypeTag: Sizable](a: A, x: Array[A], y: Array[A])(ccf: Array[A] => CC)(flags: Set[Tag] = Set.empty): Instance[A, CC] =
    new Instance(
      a,
      new CachedFn0(() => ccf(x)), implicitly[Sizable[CC]].sizeof(ccf(x)),
      new CachedFn0(() => ccf(y)), implicitly[Sizable[CC]].sizeof(ccf(y)), 
      flags
    )

  trait FromArray[A, CC] extends ((A, Array[A], Array[A]) => Instance[A, CC]) with Named {
    override def toString = f"$name from array"
  }

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

trait Touchable {
  def accesses: Int
  def name: String
  def group: String
  def path = group + ": " + name
}

/** Provides a source for individual instances we will test.
  * The variable names used are ones we can use to select
  * tests inside the generator.
  *
  * Collections that can take any generic type go in here.  Collections that can only
  * take certain types go in more specific subclasses (or, in the case of maps, an alternate trait) below.
  */
abstract class InstantiatorsOf[A]
extends Exploratory[(A, Array[A], Array[A])] {
  import Instance.Sizable
  import Tag._

  protected implicit def orderingOfA: Ordering[A]
  protected implicit def typeTagA: TypeTag[A]
  protected def allFlags: Array[Tag]
  protected val inst = Instance.over[A](allFlags: _*)

  protected implicit def sizeOfSeq[A, S[A] <: collection.Seq[A]] = new Sizable[S[A]] { def sizeof(s: S[A]) = s.length }
  protected implicit def sizeOfOnce[A, O[A] <: collection.Traversable[A]] = new Sizable[O[A]] { def sizeof(o: O[A]) = o.size }
  protected implicit def sizeOfArray[A] = new Sizable[Array[A]] { def sizeof(a: Array[A]) = a.length }
  protected implicit val sizeOfString = new Sizable[String] { def sizeof(s: String) = s.length }

  trait Touched[A, CC] extends Function0[Instance.FromArray[A, CC]] with Touchable {
    def secretly: Instance.FromArray[A, CC]
  }

  protected val registry = Vector.newBuilder[Touched[A, _]]

  object Imm extends Instance.PackagePath {
    def nickname = "Imm"
    def fullyQualified = "scala.collection.immutable"
    def C[CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Tag*)(implicit nm: sourcecode.Name): Touched[A, CC] = {
      val gen = inst.generatorCached(ccf, flags: _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Touched[A, CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[A, CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }
    val hashSet     = C(_.to[collection.immutable.HashSet], SET)
    val indexedSeq  = C(_.to[collection.immutable.IndexedSeq], SEQ)
    val iterable    = C(_.to[collection.immutable.Iterable])
    val linearSeq   = C(_.to[collection.immutable.LinearSeq], SEQ)
    val list        = C(_.toList, SEQ)
    val queue       = C(_.to[collection.immutable.Queue], SEQ)
    val seq         = C(_.to[collection.immutable.Seq], SEQ)
    val set         = C(_.toSet, SET)
    val sortedSet   = C(_.to[collection.immutable.SortedSet], SET)
    val stream      = C(_.to[Stream], SEQ)
    val traversable = C(_.to[collection.immutable.Traversable])
    val treeSet     = C(_.to[collection.immutable.TreeSet], SET)
    val vector      = C(_.toVector, SEQ)
  }

  object Mut extends Instance.PackagePath {
    def nickname = "Mut"
    def fullyQualified = "scala.collection.mutable"
    def C[CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Tag*)(implicit nm: sourcecode.Name): Touched[A, CC] = {
      val gen = inst.generator(ccf, flags: _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Touched[A, CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[A, CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }
    val array        = C(_.clone, SEQ, ARR)
    val arrayBuffer  = C(_.to[collection.mutable.ArrayBuffer], SEQ)
    val arraySeq     = C(_.to[collection.mutable.ArraySeq], SEQ)
    val arrayStack   = C(_.to[collection.mutable.ArrayStack], SEQ)
    val buffer       = C(_.to[collection.mutable.Buffer], SEQ)
    val hashSet      = C(_.to[collection.mutable.HashSet], SET)
    val indexedSeq   = C(_.to[collection.mutable.IndexedSeq], SEQ)
    val iterable     = C(_.to[collection.mutable.Iterable])
    val linearSeq    = C(_.to[collection.mutable.LinearSeq], SEQ)
    val linkedHashSet= C(_.to[collection.mutable.LinkedHashSet], SET)
    val listBuffer   = C(_.to[collection.mutable.ListBuffer], SEQ)
    val priorityQueue= C(_.to[collection.mutable.PriorityQueue])
    val queue        = C(_.to[collection.mutable.Queue], SEQ)
    val seq          = C(_.to[collection.mutable.Seq], SEQ)
    val treeSet      = C(_.to[collection.mutable.TreeSet], SET)
    // val unrolledBuffer = C(_.to[collection.mutable.UnrolledBuffer], SEQ)
    val wrappedArray = C(_.clone: collection.mutable.WrappedArray[A], SEQ)
  }

  def possible_a: Array[A]
  def possible_x: Array[Array[A]]
  def possible_y: Array[Array[A]]

  val sizes = Array(possible_a.length, possible_x.length, possible_y.length)
  
  def lookup(ixs: Array[Int]): Option[(A, Array[A], Array[A])] =
    if (!validate(ixs)) None
    else Some((possible_a(ixs(0)), possible_x(ixs(1)), possible_y(ixs(2))))

  def force(): Any  // Makes sure all the objects that are used are loaded.

  lazy val all = {
    force()
    registry.result
  }
}

/** Instantiators for map types where both keys and values can be anything.
  *
  * Maps with restrictions on keys or values go in more specific subclasses below.
  */
trait InstantiatorsOfKV[K, V] extends Exploratory[((K, V), Array[(K, V)], Array[(K, V)])] { self: InstantiatorsOf[(K, V)] =>
  import Instance.Sizable
  import Tag._

  protected implicit def orderingOfK: Ordering[K]

  protected implicit def typeTagK: TypeTag[K]
  protected implicit def typeTagV: TypeTag[V]
  protected val kvInst = Instance.over[(K, V)](allFlags: _*)
  protected implicit def sizeOfMap[K, V, M[K, V] <: collection.Map[K, V]] = new Sizable[M[K, V]] { def sizeof(m: M[K, V]) = m.size }

  object ImmKV extends Instance.PackagePath {
    def nickname = "ImmKV"
    def fullyQualified = "scala.collection.immutable"
    def C[CC: TypeTag: Sizable](ccf: Array[(K, V)] => CC, flags: Tag*)(implicit nm: sourcecode.Name): Touched[(K, V), CC] = {
      val gen = kvInst.generatorCached(ccf, (MAP +: flags): _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Touched[(K, V), CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[(K, V), CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }
    val hashMap =   C({ a => val mb = collection.immutable.HashMap.newBuilder[K, V];   for (kv <- a) mb += kv; mb.result })
    val listMap =   C({ a => val mb = collection.immutable.ListMap.newBuilder[K, V];   for (kv <- a) mb += kv; mb.result })
    val sortedMap = C({ a => val mb = collection.immutable.SortedMap.newBuilder[K, V]; for (kv <- a) mb += kv; mb.result })
    val treeMap =   C({ a => val mb = collection.immutable.TreeMap.newBuilder[K, V];   for (kv <- a) mb += kv; mb.result })
  }

  object MutKV extends Instance.PackagePath {
    def nickname = "MutKV"
    def fullyQualified = "scala.collection.mutable"
    def C[CC: TypeTag: Sizable](ccf: Array[(K, V)] => CC, flags: Tag*)(implicit nm: sourcecode.Name): Touched[(K, V), CC] = {
      val gen = kvInst.generator(ccf, (MAP +: flags): _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Touched[(K, V), CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[(K, V), CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }
    val hashMap =       C({ a => val m = new collection.mutable.HashMap[K, V];       for (kv <- a) m += kv; m })
    val listMap =       C({ a => val m = new collection.mutable.ListMap[K, V];       for (kv <- a) m += kv; m })
    val linkedHashMap = C({ a => val m = new collection.mutable.LinkedHashMap[K, V]; for (kv <- a) m += kv; m })
    val openHashMap =   C({ a => val m = new collection.mutable.OpenHashMap[K, V];   for (kv <- a) m += kv; m })
    val sortedMap =     C({ a => val m = collection.mutable.SortedMap.empty[K, V];   for (kv <- a) m += kv; m })
    val treeMap =       C({ a => val m = new collection.mutable.TreeMap[K, V];       for (kv <- a) m += kv; m })
    val weakHashMap =   C({ a => val m = new collection.mutable.WeakHashMap[K, V];   for (kv <- a) m += kv; m })
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
  import Instance.Sizable
  import Tag._

  protected implicit def orderingOfA = OrderingSource.orderingOfInt
  protected implicit def typeTagA = TypeTagSource.typeTagInt
  protected def allFlags = Array(INT)

  protected implicit val sizeOfRange = new Sizable[collection.immutable.Range] { def sizeof(r: collection.immutable.Range) = r.size }
  protected implicit val sizeOfIBitSet = new Sizable[collection.immutable.BitSet] { def sizeof(s: collection.immutable.BitSet) = s.size }
  protected implicit val sizeOfMBitSet = new Sizable[collection.mutable.BitSet] { def sizeof(s: collection.mutable.BitSet) = s.size }
  object ImmInt extends Instance.PackagePath {
    // If we have other (String, _) types, move this out into a trait
    def nickname = "ImmInt"
    def fullyQualified = "scala.collection.immutable"
    def C[CC: TypeTag: Sizable](ccf: Array[Int] => CC, flags: Tag*)(implicit nm: sourcecode.Name): Touched[Int, CC] = {
      val gen = inst.generatorCached(ccf, (NOG +: flags): _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Touched[Int, CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[Int, CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }
    val bitSet = C({ a => val b = collection.immutable.BitSet.newBuilder; a.foreach{ x => if (x >= 0) b += x }; b.result }, SET)
    //val range = C({ a => if (a.length % 3 == 0) 0 until a.length else 0 to a.length })
  }
  object MutInt extends Instance.PackagePath {
    // If we have other (String, _) types, move this out into a trait
    def nickname = "MutInt"
    def fullyQualified = "scala.collection.mutable"
    def C[CC: TypeTag: Sizable](ccf: Array[Int] => CC, flags: Tag*)(implicit nm: sourcecode.Name): Touched[Int, CC] = {
      val gen = inst.generatorCached(ccf, (NOG +: flags): _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Touched[Int, CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[Int, CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }
    val bitSet = C({ a => val b = new collection.mutable.BitSet; a.foreach{ x => if (x >= 0) b += x }; b }, SET)
  }

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
    Array.fill(254)(42),
    Array.range(0,811)
  )
  lazy val possible_y = possible_x

  val force = Imm :: Mut :: ImmInt :: MutInt :: Nil
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

  val force = Imm :: Mut :: Nil
}

object InstantiatorsOfLongStr extends InstantiatorsOf[(Long, String)] with InstantiatorsOfKV[Long, String] {
  import Instance.Sizable
  import Tag._

  protected implicit def orderingOfA = OrderingSource.orderingOfLongString
  protected implicit def orderingOfK = OrderingSource.orderingOfLong
  protected implicit def typeTagA = TypeTagSource.typeTagLongString
  protected implicit def typeTagK = TypeTagSource.typeTagLong
  protected implicit def typeTagV = TypeTagSource.typeTagString
  protected def allFlags = Array[Tag]()

  protected implicit val sizeOfLongMap_Long_String = 
    new Sizable[collection.mutable.LongMap[String]] { 
      def sizeof(m: collection.mutable.LongMap[String]) = m.size 
    }
  object MutLongV extends Instance.PackagePath {
    // If we have other (String, _) types, move this out into a trait
    def nickname = "MutLongV"
    def fullyQualified = "scala.collection.mutable"
    def C[CC: TypeTag: Sizable](ccf: Array[(Long, String)] => CC, flags: Tag*)(implicit nm: sourcecode.Name): Touched[(Long, String), CC] = {
      val gen = kvInst.generator(ccf, (MAP +: flags): _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Touched[(Long, String), CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[(Long, String), CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }
    val longMap = C({ a => val m = new collection.mutable.LongMap[String];     for (kv <- a) m += kv; m })
  }

  lazy val possible_a = Array(3L -> "wish")
  lazy val possible_x = Array(
    Array.empty[(Long, String)],
    possible_a,
    Array(1L -> "herring", 2L -> "cod", 3L -> "salmon")
  )
  lazy val possible_y = possible_x

  val force = ImmKV :: MutKV :: MutLongV :: Nil
}

object InstantiatorsOfStrLong extends InstantiatorsOf[(String, Long)] with InstantiatorsOfKV[String, Long] {
  import Instance.Sizable
  import Tag._

  protected implicit def orderingOfA = OrderingSource.orderingOfStringLong
  protected implicit def orderingOfK = OrderingSource.orderingOfString
  protected implicit def typeTagA = TypeTagSource.typeTagStringLong
  protected implicit def typeTagK = TypeTagSource.typeTagString
  protected implicit def typeTagV = TypeTagSource.typeTagLong
  protected def allFlags = Array[Tag]()

  protected implicit val sizeOfAnyRefMap_String_Long = 
    new Sizable[collection.mutable.AnyRefMap[String, Long]] { 
      def sizeof(m: collection.mutable.AnyRefMap[String, Long]) = m.size 
    }
  object MutKrefV extends Instance.PackagePath {
    // If we have other (String, _) types, move this out into a trait
    def nickname = "MutKrefV"
    def fullyQualified = "scala.collection.mutable"
    def C[CC: TypeTag: Sizable](ccf: Array[(String, Long)] => CC, flags: Tag*)(implicit nm: sourcecode.Name): Touched[(String, Long), CC] = {
      val gen = kvInst.generator(ccf, (MAP +: flags): _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Touched[(String, Long), CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[(String, Long), CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }
    val anyRefMap = C({ a => val m = new collection.mutable.AnyRefMap[String, Long];     for (kv <- a) m += kv; m })
  }

  lazy val possible_a = Array("wish" -> 3L)
  lazy val possible_x = Array(
    Array.empty[(String, Long)],
    possible_a,
    Array("herring" -> 1L, "cod" -> 2L, "salmon" -> 3L)
  )
  lazy val possible_y = possible_x

  val force = ImmKV :: MutKV :: MutKrefV :: Nil
}
