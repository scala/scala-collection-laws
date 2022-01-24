package laws

import scala.language.higherKinds

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag


/** Provides a source for individual instances we will test.
  * The variable names used are ones we can use to select
  * tests inside the generator.
  *
  * Collections that can take any generic type go in here.  Collections that can only
  * take certain types go in more specific subclasses (or, in the case of maps, an alternate trait) below.
  *
  * The organization of the instantiators is rather repetitive, but favors transparency of
  * how the underlying collection is being generated.
  *
  * Each collection gets a single line inside a `C` creator utility method which hooks up a function
  * that generates the appropriate collection from an array, and sets any appropriate flags.  This
  * utility method also registers the collection.
  *
  * To organize the collections by common packages, inner objects are used.  These supply both a nickname
  * for the package, used in the test file name, and the full package path.  Since objects are lazily
  * initialized, the package-specific objects then need to be evaluated.  This is both an advantage and a
  * disadvantage: you can have more instantiators specified than you ever create, but it is also possible
  * to miss them entirely by missing the initializer (see `force`).
  */
abstract class InstantiatorsOf[A]
extends Exploratory[(A, Array[A], Array[A])] {
  import Flag._

  protected implicit def orderingOfA: Ordering[A]   // Require some kind of ordering (even a dumb one) for all element types
  protected implicit def typeTagA: TypeTag[A]       // TypeTag that gives us information about the element
  protected implicit def classTagA: ClassTag[A]     // ClassTag that allows us to do things with arrays

  protected def allFlags: Array[Flag]
  protected val inst = Instance.flagged[A](collection.immutable.ArraySeq.unsafeWrapArray(allFlags): _*)   // Add all the flags specified in `allFlags`

  // Ways to get sizes of different kinds of collections
  protected implicit def sizeOfSeq[A, S[A] <: collection.Seq[A]] = new Sizable[S[A]] { def sizeof(s: S[A]) = s.length }
  protected implicit def sizeOfIterable[A, O[A] <: collection.Iterable[A]] = new Sizable[O[A]] { def sizeof(o: O[A]) = o.size }
  protected implicit def sizeOfArray[A] = new Sizable[Array[A]] { def sizeof(a: Array[A]) = a.length }
  protected implicit val sizeOfString = new Sizable[String] { def sizeof(s: String) = s.length }
  protected implicit def sizeOfIterator[A] = new Sizable[Iterator[A]] { def sizeof(i: Iterator[A]) = i match {
    case iks: Root.IteratorKnowsSize[_] => iks.knownSize
    case _                              => throw new Exception("Actually, `Iterator` hasn't the foggiest idea what its size is.")
  }}

  /** Marks when an instantiator is used (for completeness checking) */
  trait Deployed[A, CC] extends Function0[Instance.FromArray[A, CC]] with Instance.Deployed { self =>
    def secretly: Instance.FromArray[A, CC]

    // Forwarder that allows us to add a source of enriched methods
    def moreMethods(mc: MethodChecker): Deployed[A, CC] = new Deployed[A, CC] {
      def accesses = self.accesses
      def name = self.name
      def group = self.group
      override def path = self.path
      def apply() = self.apply().moreMethods(mc)
      def secretly = self.secretly.moreMethods(mc)
    }
  }

  protected val registry = Vector.newBuilder[Deployed[A, _]]

  object Imm extends Instance.PackagePath {
    def nickname = "Imm"
    def fullyQualified = "scala.collection.immutable"
    def C[CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Flag*)(implicit nm: sourcecode.Name): Deployed[A, CC] = {
      val gen = inst.cacheWith(ccf, flags: _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Deployed[A, CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[A, CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }

    // MUST use lower-camel-cased collection class name for code generator to work properly!
    val arraySeq    = C(collection.immutable.ArraySeq unsafeWrapArray _, SEQ, SPLITS)
    val hashSet     = C(_.to(collection.immutable.HashSet), SET, SPLITS)
    val indexedSeq  = C(_.to(collection.immutable.IndexedSeq), SEQ, SPLITS)
    val iterable    = C(_.to(collection.immutable.Iterable))
    val lazyList    = C(
      a => collection.immutable.LazyList.from(0).takeWhile(_ < a.length).map(i => a(i)),
      SEQ, INDEF
    )
    val linearSeq   = C(_.to(collection.immutable.LinearSeq), SEQ)
    val list        = C(_.toList, SEQ)
    val queue       = C(_.to(collection.immutable.Queue), SEQ)
    val seq         = C(_.to(collection.immutable.Seq), SEQ)
    val set         = C(_.toSet, SET)
    val sortedSet   = C(_.to(collection.immutable.SortedSet), SET, ORDERLY)
    val stream      = C(
      a => collection.immutable.Stream.from(0).takeWhile(_ < a.length).map(i => a(i)),
      SEQ, INDEF
    )
    val traversable = C(_.to(collection.immutable.Traversable))
    val treeSet     = C(_.to(collection.immutable.TreeSet), SET, ORDERLY, SPLITS)
    val vector      = C(_.toVector, SEQ, SPLITS)
  }

  object Mut extends Instance.PackagePath {
    def nickname = "Mut"
    def fullyQualified = "scala.collection.mutable"
    def C[CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Flag*)(implicit nm: sourcecode.Name): Deployed[A, CC] = {
      val gen = inst.makeWith(ccf, flags: _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Deployed[A, CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[A, CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }

    // MUST use lower-camel-cased collection class name for code generator to work properly!
    val array        = C(_.clone, SEQ, ARRAY, SORTWITH_INT_CCE, SPLITS).moreMethods(MethodChecker.from[collection.ArrayOps[A]])
    val arrayBuffer  = C(_.to(collection.mutable.ArrayBuffer), SEQ, SPLITS)
    val arrayDeque   = C(_.to(collection.mutable.ArrayDeque), SEQ, SPLITS, QUEUE_SLIDE_11440)
    val arraySeq     = C(_.to(collection.mutable.ArraySeq), SEQ, SPLITS)
    val arrayStack   = C(_.to(collection.mutable.ArrayStack), SEQ, SPLITS, QUEUE_SLIDE_11440)
    val buffer       = C(_.to(collection.mutable.Buffer), SEQ)
    val hashSet      = C(_.to(collection.mutable.HashSet), SET, SPLITS)
    val indexedSeq   = C(_.to(collection.mutable.IndexedSeq), SEQ, SPLITS)
    val iterable     = C(_.to(collection.mutable.Iterable))
    val linkedHashSet= C(_.to(collection.mutable.LinkedHashSet), SET, SPLITS, STAGGER)
    val listBuffer   = C(_.to(collection.mutable.ListBuffer), SEQ, LISTBUF_PIP_11438)
    val priorityQueue= C(_.to(collection.mutable.PriorityQueue), ORDERLY, PRIORITYQUEUE_IS_SPECIAL, PQ_MIP_NPE_11439)
    val queue        = C(_.to(collection.mutable.Queue), SEQ, SPLITS, QUEUE_SLIDE_11440)
    val seq          = C(_.to(collection.mutable.Seq), SEQ)
    val stack        = C(_.to(collection.mutable.Stack), SPLITS, QUEUE_SLIDE_11440)
    val treeSet      = C(_.to(collection.mutable.TreeSet), SET, SPLITS, ORDERLY)
    // val unrolledBuffer = C(_.to(collection.mutable.UnrolledBuffer), SEQ)  // Unrolled buffer is weird!
    val wrappedArray = C(_.clone: collection.mutable.WrappedArray[A], SEQ, SPLITS)
  }

  object Conv extends Instance.PackagePath {
    def nickname = "Conv"
    def fullyQualified = "scala.jdk"
    def C[CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Flag*)(implicit nm: sourcecode.Name): Deployed[A, CC] = {
      val gen = inst.makeWith(ccf, flags: _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Deployed[A, CC] {
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + "in " + nickname
        def apply(): Instance.FromArray[A, CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }

    // TODO--use the generic builder in a productive way; hard because it's Ops-like not its own collection
    // val accumulator = C(a => collection.convert.Accumulator from a, SEQ)

    val anyAccumulator = C(a => scala.jdk.AnyAccumulator(a.toSeq: _*), SEQ, SPLITS)
  }

  object Root extends Instance.PackagePath {
    def nickname = "Root"
    def fullyQualified = "scala.collection"
    def C[CC: TypeTag: Sizable](ccf: Array[A] => CC, flags: Flag*)(implicit nm: sourcecode.Name): Deployed[A, CC] = {
      val gen = inst.makeWith(ccf, flags: _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Deployed[A, CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[A, CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }

    class IteratorKnowsSize[A](a: Array[A]) extends scala.collection.AbstractIterator[A] {
      private[this] var i = 0
      override def knownSize = a.length - i
      def hasNext = i < a.length
      def next =
        if (!hasNext) Iterator.empty.next()
        else {
          val ans = a(i);
          i += 1
          ans
        }
    }

    // MUST use lower-camel-cased collection class name for code generator to work properly!
    val iterator       = C(a => (new IteratorKnowsSize[A](a)): Iterator[A], ONCE, INDEF)
    val view           = C(a => a.to(collection.immutable.Vector).view: scala.collection.View[A], VIEW)

    // These don't work because they take arguments of a different type than they are themselves
    // val indexedSeqView = C(a => a.view: scala.collection.IndexedSeqView[A])
    // val seqView        = C(a => a.to(collection.immutable.List).view: scala.collection.SeqView[A])
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
  import Flag._

  protected implicit def orderingOfK: Ordering[K]

  protected implicit def typeTagK: TypeTag[K]
  protected implicit def typeTagV: TypeTag[V]
  protected val kvInst = Instance.flagged[(K, V)](collection.immutable.ArraySeq.unsafeWrapArray(allFlags): _*)
  protected implicit def sizeOfMap[K, V, M[K, V] <: collection.Map[K, V]] = new Sizable[M[K, V]] { def sizeof(m: M[K, V]) = m.size }

  object ImmKV extends Instance.PackagePath {
    def nickname = "ImmKV"
    def fullyQualified = "scala.collection.immutable"
    def C[CC: TypeTag: Sizable](ccf: Array[(K, V)] => CC, flags: Flag*)(implicit nm: sourcecode.Name): Deployed[(K, V), CC] = {
      val gen = kvInst.cacheWith(ccf, (MAP +: flags): _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Deployed[(K, V), CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[(K, V), CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }

    // MUST use lower-camel-cased collection class name for code generator to work properly!
    val hashMap =    C({ a => val mb = collection.immutable.HashMap.newBuilder[K, V];    for (kv <- a) mb += kv; mb.result }, SPLITS)
    val listMap =    C({ a => val mb = collection.immutable.ListMap.newBuilder[K, V];    for (kv <- a) mb += kv; mb.result })
    val sortedMap =  C({ a => val mb = collection.immutable.SortedMap.newBuilder[K, V];  for (kv <- a) mb += kv; mb.result })
    val treeMap =    C({ a => val mb = collection.immutable.TreeMap.newBuilder[K, V];    for (kv <- a) mb += kv; mb.result }, SPLITS)
    val treeSeqMap = C({ a => val mb = collection.immutable.TreeSeqMap.newBuilder[K, V]; for (kv <- a) mb += kv; mb.result }, INSORD)      
    val vectorMap =  C({ a => val mb = collection.immutable.VectorMap.newBuilder[K, V];  for (kv <- a) mb += kv; mb.result }, INSORD, SPLITS, INEFFICIENT_BUG)
  }

  object MutKV extends Instance.PackagePath {
    def nickname = "MutKV"
    def fullyQualified = "scala.collection.mutable"
    def C[CC: TypeTag: Sizable](ccf: Array[(K, V)] => CC, flags: Flag*)(implicit nm: sourcecode.Name): Deployed[(K, V), CC] = {
      val gen = kvInst.makeWith(ccf, (MAP +: flags): _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Deployed[(K, V), CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[(K, V), CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }

    // MUST use lower-camel-cased collection class name for code generator to work properly!
    import collection.mutable.CollisionProofHashMap
    val collisionProofHashMap =
                        C({ a => val m = new CollisionProofHashMap[K, V];            for (kv <- a) m += kv; m }, CPMH_TYPE_11449)
    val hashMap =       C({ a => val m = new collection.mutable.HashMap[K, V];       for (kv <- a) m += kv; m }, SPLITS)
    val listMap =       C({ a => val m = new collection.mutable.ListMap[K, V];       for (kv <- a) m += kv; m })
    val linkedHashMap = C({ a => val m = new collection.mutable.LinkedHashMap[K, V]; for (kv <- a) m += kv; m }, INSORD, STAGGER)
    val openHashMap =   C({ a => val m = new collection.mutable.OpenHashMap[K, V];   for (kv <- a) m += kv; m })
    val sortedMap =     C({ a => val m = collection.mutable.SortedMap.empty[K, V];   for (kv <- a) m += kv; m })
    val treeMap =       C({ a => val m = new collection.mutable.TreeMap[K, V];       for (kv <- a) m += kv; m }, SPLITS)
    val weakHashMap =   C({ a => val m = new collection.mutable.WeakHashMap[K, V];   for (kv <- a) m += kv; m })
  }
}

/** Default explicit orderings for the element types we have */
object OrderingSource {
  val orderingOfLong = implicitly[Ordering[Long]]
  val orderingOfInt = implicitly[Ordering[Int]]
  val orderingOfString = implicitly[Ordering[String]]
  val orderingOfLongString = implicitly[Ordering[(Long, String)]]
  val orderingOfStringLong = implicitly[Ordering[(String, Long)]]
}

/** Default explicit type tags for the element types we have */
object TypeTagSource {
  val typeTagInt = implicitly[TypeTag[Int]]
  val typeTagLong = implicitly[TypeTag[Long]]
  val typeTagString = implicitly[TypeTag[String]]
  val typeTagLongString = implicitly[TypeTag[(Long, String)]]
  val typeTagStringLong = implicitly[TypeTag[(String, Long)]]
}

/** Default explicit class tags for the element types we have */
object ClassTagSource {
  val classTagInt = implicitly[ClassTag[Int]]
  val classTagString = implicitly[ClassTag[String]]
  val classTagLongString = implicitly[ClassTag[(Long, String)]]
  val classTagStringLong = implicitly[ClassTag[(String, Long)]]
}

/** Instantiates collections with an `Int` element type.*/
object InstantiatorsOfInt extends InstantiatorsOf[Int] {
  import Flag._

  protected implicit def orderingOfA = OrderingSource.orderingOfInt
  protected implicit def typeTagA = TypeTagSource.typeTagInt
  protected implicit def classTagA = ClassTagSource.classTagInt
  protected def allFlags = Array(INT)

  protected implicit val sizeOfRange = new Sizable[collection.immutable.Range]    { def sizeof(r: collection.immutable.Range) = r.size }
  protected implicit val sizeOfIBitSet = new Sizable[collection.immutable.BitSet] { def sizeof(s: collection.immutable.BitSet) = s.size }
  protected implicit val sizeOfMBitSet = new Sizable[collection.mutable.BitSet]   { def sizeof(s: collection.mutable.BitSet) = s.size }
  protected implicit val sizeOfIAccum = new Sizable[scala.jdk.IntAccumulator]     { def sizeof(a: scala.jdk.IntAccumulator) = a.size }

  /** Extra instantiators specific to Ints in immutable collections */
  object ImmInt extends Instance.PackagePath {
    // If we have other (String, _) types, move this out into a trait
    def nickname = "ImmInt"
    def fullyQualified = "scala.collection.immutable"
    def C[CC: TypeTag: Sizable](ccf: Array[Int] => CC, flags: Flag*)(implicit nm: sourcecode.Name): Deployed[Int, CC] = {
      val gen = inst.cacheWith(ccf, flags: _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Deployed[Int, CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[Int, CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }

    // MUST use lower-camel-cased collection clasTs name for code generator to work properly!
    val bitSet = C(
      { a => val b = collection.immutable.BitSet.newBuilder; a.foreach{ x => if (x >= 0) b += x }; b.result },
      SET, ORDERLY, SPECTYPE, BITSET, SPLITS, BITSET_MAP_AMBIG, BITSET_ZIP_AMBIG
    )
    //val range = C({ a => if (a.length % 3 == 0) 0 until a.length else 0 to a.length })
  }

  /** Extra instantiators sepcific to Ints in mutable collections */
  object MutInt extends Instance.PackagePath {
    // If we have other (String, _) types, move this out into a trait
    def nickname = "MutInt"
    def fullyQualified = "scala.collection.mutable"
    def C[CC: TypeTag: Sizable](ccf: Array[Int] => CC, flags: Flag*)(implicit nm: sourcecode.Name): Deployed[Int, CC] = {
      val gen = inst.makeWith(ccf, flags: _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Deployed[Int, CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[Int, CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }

    // MUST use lower-camel-cased collection class name for code generator to work properly!
    val bitSet = C(
      { a => val b = new collection.mutable.BitSet; a.foreach{ x => if (x >= 0) b += x }; b },
      SET, ORDERLY, SPECTYPE, BITSET, SPLITS, BITSET_MAP_AMBIG, BITSET_ZIP_AMBIG
    )
  }

  object ConvInt extends Instance.PackagePath {
    def nickname = "ConvInt"
    def fullyQualified = "scala.jdk"
    def C[CC: TypeTag: Sizable](ccf: Array[Int] => CC, flags: Flag*)(implicit nm: sourcecode.Name): Deployed[Int, CC] = {
      val gen = inst.makeWith(ccf, flags: _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Deployed[Int, CC] {
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + "in " + nickname
        def apply(): Instance.FromArray[Int, CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }

    val intAccumulator = C(a => scala.jdk.IntAccumulator(a.toSeq: _*), SEQ, SPECTYPE, SPLITS, ACC_SPEC)
  }

  /** Singleton `Int` values to test */
  lazy val possible_a = Array(0, 1, 2, 3, 4, 5, 7, 8, 9, 15, 16, 17, 23, 31, 47, 152, 3133, 1294814, -1, -2, -6, -19, -1915, -19298157)

  /** Collection contents to test (for the primary `x` collection) */
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
  /** The `y` collection can take on the same values as the `x` */
  lazy val possible_y = possible_x

  /** This is important!  This registers the collections that you actually want to have available! */
  val force = Imm :: Mut :: Root :: ImmInt :: MutInt :: Nil
}

/** Instantiates collections with a `String` element type.*/
object InstantiatorsOfStr extends InstantiatorsOf[String] {
  import Flag._

  protected implicit def orderingOfA = OrderingSource.orderingOfString
  protected implicit def typeTagA = TypeTagSource.typeTagString
  protected implicit def classTagA = ClassTagSource.classTagString
  protected def allFlags = Array(STR)

  /** Singleton `String` values to test */
  lazy val possible_a = Array(
    "", "0", "one", "salmon", "\u0000\u0000\u0000\u0000", "the quick brown fox jumps over the lazy dog", "\u1517\u1851..!"
  )

  /** Collection contents to test (for the primary `x` collection) */
  lazy val possible_x = Array(
    Array.empty[String],
    Array(possible_a(1)),
    Array(possible_a(3)),
    possible_a,
    Array("0", "1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1"),
    Array.range(-44, 45).map(_.toString),
    Array.fill(184)("herring")
  )
  /** The `y` collection can take on the same values as the `x` */
  lazy val possible_y = possible_x

  /** This is important!  This registers the collections that you actually want to have available! */
  val force = Imm :: Mut :: Root :: Nil
}

/** Instantiates `(Long, String)` pairs for use with maps.
  *
  * Other collections could be tested with this tuple as the element type, but they're not.
  */
object InstantiatorsOfLongStr extends InstantiatorsOf[(Long, String)] with InstantiatorsOfKV[Long, String] {
  import Flag._

  protected implicit def orderingOfA = OrderingSource.orderingOfLongString
  protected implicit def orderingOfK = OrderingSource.orderingOfLong
  protected implicit def typeTagA = TypeTagSource.typeTagLongString
  protected implicit def typeTagK = TypeTagSource.typeTagLong
  protected implicit def typeTagV = TypeTagSource.typeTagString
  protected implicit def classTagA = ClassTagSource.classTagLongString
  protected def allFlags = Array[Flag]()

  protected implicit val sizeOfLongMap_Long_String = 
    new Sizable[collection.mutable.LongMap[String]] { 
      def sizeof(m: collection.mutable.LongMap[String]) = m.size 
    }

  /** Instantiators for special mutable maps requiring a `Long` key type */
  object MutLongV extends Instance.PackagePath {
    // If we have other (String, _) types, move this out into a trait
    def nickname = "MutLongV"
    def fullyQualified = "scala.collection.mutable"
    def C[CC: TypeTag: Sizable](ccf: Array[(Long, String)] => CC, flags: Flag*)(implicit nm: sourcecode.Name): Deployed[(Long, String), CC] = {
      val gen = kvInst.makeWith(ccf, (MAP +: flags): _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Deployed[(Long, String), CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[(Long, String), CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }
    val longMap = C({ a => val m = new collection.mutable.LongMap[String];     for (kv <- a) m += kv; m }, SPECTYPE, SPLITS, INEFFICIENT_BUG)
  }

  /** Very limited set of possible singletons */
  lazy val possible_a = Array(3L -> "wish")

  /** Very limited set of possible values for arrays. */
  lazy val possible_x = Array(
    Array.empty[(Long, String)],
    possible_a,
    Array(1L -> "herring", 2L -> "cod", 3L -> "salmon"),
    Array(9L -> "nine", 3L -> "three", 7L -> "seven", -1L -> "negative", 42L -> "Adams", 6L -> "vi"),
    (0 to 44).map(i => i.toLong -> i.toString).toArray
  )
  /** The `y` collection can take on the same values as the `x` */
  lazy val possible_y = possible_x

  /** This is important!  This registers the collections that you actually want to have available!
    *
    * In particular, notice that we're only taking the key-value instantiators, so we only register maps, not all collections.
    */
  val force = ImmKV :: MutKV :: MutLongV :: Nil
}

/** Instantiates `(Long, String)` pairs for use with maps.
  *
  * Other collections could be tested with this tuple as the element type, but they're not.
  */
object InstantiatorsOfStrLong extends InstantiatorsOf[(String, Long)] with InstantiatorsOfKV[String, Long] {
  import Flag._

  protected implicit def orderingOfA = OrderingSource.orderingOfStringLong
  protected implicit def orderingOfK = OrderingSource.orderingOfString
  protected implicit def typeTagA = TypeTagSource.typeTagStringLong
  protected implicit def typeTagK = TypeTagSource.typeTagString
  protected implicit def typeTagV = TypeTagSource.typeTagLong
  protected implicit def classTagA = ClassTagSource.classTagStringLong
  protected def allFlags = Array[Flag]()

  protected implicit val sizeOfAnyRefMap_String_Long = 
    new Sizable[collection.mutable.AnyRefMap[String, Long]] { 
      def sizeof(m: collection.mutable.AnyRefMap[String, Long]) = m.size 
    }

  object MutKrefV extends Instance.PackagePath {
    // If we have other (String, _) types, move this out into a trait
    def nickname = "MutKrefV"
    def fullyQualified = "scala.collection.mutable"
    def C[CC: TypeTag: Sizable](ccf: Array[(String, Long)] => CC, flags: Flag*)(implicit nm: sourcecode.Name): Deployed[(String, Long), CC] = {
      val gen = kvInst.makeWith(ccf, (MAP +: flags): _*)(nm, implicitly[TypeTag[CC]], implicitly[Sizable[CC]])
      val ans = new Deployed[(String, Long), CC]{
        val secretly = gen
        var accesses: Int = 0
        val name = nm.value.toString
        def group = typeTagA.tpe.toString + " in " + nickname
        def apply(): Instance.FromArray[(String, Long), CC] = { accesses += 1; secretly }
      }
      registry += ans
      ans
    }
    val anyRefMap = C({ a => val m = new collection.mutable.AnyRefMap[String, Long]; for (kv <- a) m += kv; m }, SPECTYPE, SPLITS, INEFFICIENT_BUG)
  }

  lazy val possible_a = Array("wish" -> 3L)
  lazy val possible_x = Array(
    Array.empty[(String, Long)],
    possible_a,
    Array("herring" -> 1L, "cod" -> 2L, "salmon" -> 3L),
    Array("nine" -> 9L, "three" -> 3L, "seven" -> 7L, "negative" -> -1L, "Adams" -> 42L, "vi" -> 6L),
    (0 to 44).map(i => i.toString -> i.toLong).toArray
  )
  lazy val possible_y = possible_x

  /** This is important!  This registers the collections that you actually want to have available!
    *
    * In particular, notice that we're only taking the key-value instantiators, so we only register maps, not all collections.
    */
  val force = ImmKV :: MutKV :: MutKrefV :: Nil
}
