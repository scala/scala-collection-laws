package laws

//###################################################################//
// This file contains the operations to apply to collection tests of //
// various types.  That way you can plug in different operations.    //
//###################################################################//


/////////////////////////////////////////////////////////////////
// Location-logging wrapper classes for various function forms //
/////////////////////////////////////////////////////////////////

/** Wrapper class around a function that lets you tell where it came from */
class ===>[X, Y](val fn: X => Y)(implicit file: sourcecode.File, line: sourcecode.Line, nm: sourcecode.Name)
extends Named {
  def name = nm.value.toString
  override val toString = nm.value.toString + " @ " + Sourced.implicitly
  override def equals(that: Any) = that match {
    case x: ===>[_, _] => toString == x.toString
    case _             => false
  }
  override val hashCode = scala.util.hashing.MurmurHash3.stringHash(toString)
}

/** Wrapper class around a binary operation that lets you tell where it came from */
class OpFn[X](
  val ofn: (X, X) => X, val zero: Option[X], val assoc: OpFn.Associativity, val sym: OpFn.Symmetry
)(implicit file: sourcecode.File, line: sourcecode.Line, nm: sourcecode.Name) 
extends Named {
  def name = nm.value.toString
  override val toString = nm.value.toString + " @ " + Sourced.implicitly
  override def equals(that: Any) = that match {
    case x: OpFn[_] => toString == x.toString
    case _          => false
  }
  override val hashCode = scala.util.hashing.MurmurHash3.stringHash(toString)
}
object OpFn {
  sealed trait Associativity {}
  final case object Associative extends Associativity {}
  final case object Nonassociative extends Associativity {}
  sealed trait Symmetry {}
  final case object Symmetric extends Symmetry {}
  final case object Asymmetric extends Symmetry {}
}

/** Wrapper class around a partial function that lets you tell where it came from */
class ParFn[X](val pfn: PartialFunction[X, X])(implicit file: sourcecode.File, line: sourcecode.Line, nm: sourcecode.Name)
extends Named {
  def name = nm.value.toString
  override val toString = nm.value.toString + " @ " + Sourced.implicitly
  override def equals(that: Any) = that match {
    case x: ParFn[_] => toString == x.toString
    case _           => false
  }
  override val hashCode = scala.util.hashing.MurmurHash3.stringHash(toString)
}


///////////////////////////////////////////////
// The main class holding all the operations //
///////////////////////////////////////////////

/** Class that represents the ways we can transform and select data for a given element type. */
final class Ops[A, B](f0: A ===> A, g0: A ===> B, op0: OpFn[A], p0: A ===> Boolean, pf0: ParFn[A]) {
  val values = Ops.Values[A, B](f0, g0, op0, p0, pf0)

  val used = Array(false, false, false, false, false)

  /** A function that changes an element to another of the same type */
  def f: A => A = { used(0) = true; values.f.fn }

  /** A function that changes an element to another of a different type */
  def g: A => B = { used(1) = true; values.g.fn }

  /** A function that, given two elements of a type, produces a single element of that type */
  def op: (A, A) => A = { used(2) = true; values.op.ofn }

  /** A predicate that gives a true/false answer for an element */
  def p: A => Boolean = { used(3) = true; values.p.fn }

  /** A partial function that changes some elements to another of the same type */
  def pf: PartialFunction[A, A] = { used(4) = true; values.pf.pfn }

  /** The zero of `op`; throws an exception if there is no zero.  Filter out tests using `z` with Law#filter!
    *
    * Note: we're doing it this way since it's not practical to instrument the usage of something inside `Option`.
    */
  def z: A = { used(4) = true; op0.zero.get }

  def setUnused(): this.type = { java.util.Arrays.fill(used, false); this }

  def touched = used(0) || used(1) || used(2) || used(3) || used(4)

  override def equals(that: Any) = that match {
    case o: Ops[_, _] => values == o.values
    case _            => false
  }

  override def hashCode = values.hashCode

  override lazy val toString = {
    val parts = Array(values.f.toString, values.g.toString, values.op.toString, values.p.toString, values.pf.toString)
    val pad = parts.map(_.indexOf('@')).max
    val paddedParts =
      if (pad < 0) parts
      else parts.map{ s =>
        val i = s.indexOf('@')
        if (i <= 0 || i >= pad) s
        else s.take(i-1) + (" "*(pad - i)) + s.drop(i-1)
      }
    paddedParts.mkString("Ops\n  ", "\n  ", "")
  }
}
object Ops {
  final case class Values[A, B](f: A ===> A, g: A ===> B, op: OpFn[A], p: A ===> Boolean, pf: ParFn[A]) { self =>
    object unwrap {
      def f  = self.f.fn
      def g  = self.g.fn
      def op = self.op.ofn
      def p  = self.p.fn
      def pf = self.pf.pfn
    }
  }

  def apply[A, B](f: A ===> A, g: A ===> B, op: OpFn[A], p: A ===> Boolean, pf: ParFn[A]): Ops[A, B] = new Ops(f, g, op, p, pf)
}


/////////////////////////////////////////
// Elaboration of particular behaviors //
/////////////////////////////////////////

trait Variants[A] {
  type Item = A
  private[this] val myRegistered = collection.mutable.ArrayBuffer.empty[Item]
  private[this] var myCachedArray: Option[Array[A]] = None

  final def has(item: Item): Item = { myRegistered += item; item }

  final def all(implicit ev: reflect.ClassTag[Item]): Array[Item] =
    myCachedArray match {
      case None =>
        val a = new Array[Item](myRegistered.length)
        var i = 0; while (i < a.length) { a(i) = myRegistered(i); i += 1 }
        myCachedArray = Some(a)
        a
      case Some(a) =>
        a
    }

  final def index(i: Int)(implicit ev: reflect.ClassTag[Item]): Item = all(ev).apply(i)
}

object IntFns extends Variants[Int ===> Int] {
  val plusOne   = this has new Item(_ + 1)
  val quadratic = this has new Item(i => i*i - 3*i + 1)
}

object StrFns extends Variants[String ===> String] {
  val upper = this has new Item(_.toUpperCase)
  val fishy = this has new Item(s => f"<$s-<")
}

object IntToLongs extends Variants[Int ===> Long] {
  val bit33 = this has new Item(i => 0x200000000L | i)
  val cast  = this has new Item(i => i.toLong)
}

object StrToOpts extends Variants[String ===> Option[String]] {
  val natural = this has new Item(s => Option(s).filter(_.length > 0))
  val letter  = this has new Item(s => Option(s.filter(_.isLetter)).filter(_.length > 0))
}

object IntOpFns extends Variants[OpFn[Int]] {
  val summation = this has new Item(_ + _, Some(0), OpFn.Associative, OpFn.Symmetric)
  val multiply  = this has new Item((i, j) => i*j - 2*i - 3*j + 4, None, OpFn.Nonassociative, OpFn.Asymmetric)
}

object StrOpFns extends Variants[OpFn[String]] {
  val concat     = this has new Item(_ + _, Some(""), OpFn.Associative, OpFn.Asymmetric)
  val interleave = this has new Item((s, t) => (s zip t).map{ case (l,r) => f"$l$r" }.mkString, None, OpFn.Nonassociative, OpFn.Asymmetric)
}

object IntPreds extends Variants[Int ===> Boolean] {
  val mod3   = this has new Item(i => (i%3) == 0)
  val always = this has new Item(_ => true)
  val never  = this has new Item(_ => false)
}

object StrPreds extends Variants[String ===> Boolean] {
  val increasing = this has new Item(s => s.length < 2 || s(0) <= s(s.length-1))
  val always     = this has new Item(_ => true)
  val never      = this has new Item(_ => false)
}

object IntParts extends Variants[ParFn[Int]] {
  val halfEven    = this has new Item({ case x if (x % 2) == 0 => x / 2 })
  val identical   = this has new Item({ case x => x })
  val uninhabited = this has new Item(Function.unlift((i: Int) => None: Option[Int]))
}

object StrParts extends Variants[ParFn[String]] {
  val oddMirror   = this has new Item({ case x if (x.length % 2) == 1 => x.reverse })
  val identical   = this has new Item({ case x => x })
  val uninhabited = this has new Item(Function.unlift((s: String) => None: Option[String]))
}

class OpsExplorer[A, B](
  varFns: Variants[A ===> A],
  varToBs: Variants[A ===> B],
  varOpFns: Variants[OpFn[A]],
  varPreds: Variants[A ===> Boolean],
  varParts: Variants[ParFn[A]]
)
extends Exploratory[Ops[A, B]] {
  val sizes = Array(varFns.all.length, varToBs.all.length, varOpFns.all.length, varPreds.all.length, varParts.all.length)

  def lookup(ixs: Array[Int]): Option[Ops[A, B]] =
    if (!validate(ixs)) None
    else Some(Ops(varFns.index(ixs(0)), varToBs.index(ixs(1)), varOpFns.index(ixs(2)), varPreds.index(ixs(3)), varParts.index(ixs(4))))
}

object IntOpsExplorer extends OpsExplorer[Int, Long](IntFns, IntToLongs, IntOpFns, IntPreds, IntParts) {}

object StrOpsExplorer extends OpsExplorer[String, Option[String]](StrFns, StrToOpts, StrOpFns, StrPreds, StrParts) {}

object LongStrOpsExplorer extends OpsExplorer[(Long, String), (String, Long)](
  new Variants[(Long, String) ===> (Long, String)] { private[this] val inc1 = this has new Item(kv => (kv._1+1, kv._2)) },
  new Variants[(Long, String) ===> (String, Long)] { private[this] val swap = this has new Item(kv => (kv._2, kv._1)) },
  new Variants[OpFn[(Long, String)]] { private[this] val sums = this has new Item((kv, cu) => (kv._1 + cu._1, kv._2 + cu._2), None, OpFn.Nonassociative, OpFn.Asymmetric) },
  new Variants[(Long, String) ===> Boolean] { private[this] val high = this has new Item(kv => kv._1 > kv._2.length) },
  new Variants[ParFn[(Long, String)]] { private[this] val akin = this has new Item({ case (k, v) if ((k ^ v.length) & 1) == 0 => (k-2, v) })}
){}

object StrLongOpsExplorer extends OpsExplorer[(String, Long), (Long, String)](
  new Variants[(String, Long) ===> (String, Long)] { private[this] val crop = this has new Item(kv => (kv._1.drop(1), kv._2)) },
  new Variants[(String, Long) ===> (Long, String)] { private[this] val swap = this has new Item(kv => (kv._2, kv._1)) },
  new Variants[OpFn[(String, Long)]] { private[this] val sums = this has new Item((kv, cu) => (kv._1 + cu._1, kv._2 + cu._2), None, OpFn.Nonassociative, OpFn.Asymmetric) },
  new Variants[(String, Long) ===> Boolean] { private[this] val high = this has new Item(kv => kv._1.length < kv._2) },
  new Variants[ParFn[(String, Long)]] { private[this] val akin = this has new Item({ case (k, v) if ((k.length ^ v) & 1) == 0 => (k.drop(1), v) })}
){}
