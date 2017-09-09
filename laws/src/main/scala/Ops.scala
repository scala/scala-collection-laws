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
class OpFn[X](val ofn: (X, X) => X, val zero: Option[X], val sym: OpFn.Symmetry)(implicit file: sourcecode.File, line: sourcecode.Line, nm: sourcecode.Name)
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
  sealed trait Symmetry {}
  final case object Symmetric extends Symmetry {}
  final case object Nonsymmetric extends Symmetry {}  
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
final class Ops[A, B] private (
  private[laws] val f0: A ===> A,
  private[laws] val g0: A ===> B,
  private[laws] val op0: OpFn[A],
  private[laws] val p0: A ===> Boolean,
  private[laws] val pf0: ParFn[A]
) {
  private[this] var _fCount = 0
  private[this] var _gCount = 0
  private[this] var _opzCount = 0
  private[this] var _pCount = 0
  private[this] var _pfCount = 0

  /** A function that changes an element to another of the same type */
  def f: A => A = { _fCount += 1; f0.fn }

  /** A function that changes an element to another of a different type */
  def g: A => B = { _gCount += 1; g0.fn }

  /** A function that, given two elements of a type, produces a single element of that type */
  def op: (A, A) => A = { _opzCount += 1; op0.ofn }

  /** A predicate that gives a true/false answer for an element */
  def p: A => Boolean = { _pCount += 1; p0.fn }

  /** A partial function that changes some elements to another of the same type */
  def pf: PartialFunction[A, A] = { _pfCount += 1; pf0.pfn }

  /** The zero of `op`; throws an exception if there is no zero.  Test using `hasZ`.
    *
    * Note: we're doing it this way since it's not practical to instrument the usage of something inside `Option`.
    */
  def z: A = { _opzCount += 1; op0.zero.get }

  val hasZ: Boolean = op0.zero.isDefined

  def isSymOp: Boolean = op0.sym == OpFn.Symmetric

  def fCount = _fCount
  def gCount = _gCount
  def opzCount = _opzCount
  def pCount = _pCount
  def pfCount = _pfCount
  def count = Ops.Count(fCount, gCount, opzCount, pCount, pfCount)
  def resetCount: this.type = { _fCount = 0; _gCount = 0; _opzCount = 0; _pCount = 0; _pfCount = 0; this }

  class Secret {
    def f = f0.fn
    def g = g0.fn
    def op = op0.ofn
    def p = p0.fn
    def pf = pf0.pfn
    def z = op0.zero.get
  }
  /** Secretly access the functions and transformations (usage not recorded, so will not cause variants to be run) */
  val secret = new Secret

  override def equals(that: Any) = that match {
    case o: Ops[_, _] => (f0 == o.f0) && (g0 == o.g0) && (op0 == o.op0) && (p0 == o.p0) && (pf0 == o.pf0)
  }

  override def hashCode = {
    import scala.util.hashing.MurmurHash3._
    import java.lang.System.{identityHashCode => h}
    finalizeHash(mixLast(mix(mix(mix(h(f0), h(g0)), h(op0)), h(p0)), h(pf0)), 5)
  }

  override lazy val toString = {
    val parts = Array(f0.toString, g0.toString, op0.toString, p0.toString, pf0.toString)
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
  case class Count(fCount: Int, gCount: Int, opCount: Int, pCount: Int, pfCount: Int) {
    def -(that: Count) = new Count(
      fCount  - that.fCount,
      gCount  - that.gCount,
      opCount - that.opCount,
      pCount  - that.pCount,
      pfCount - that.pfCount
    )
  }

  def apply[A, B](f: A ===> A, g: A ===> B, op: OpFn[A], p: A ===> Boolean, pf: ParFn[A]) =
    new Ops(f, g, op, p, pf)
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
  val summation = this has new Item(_ + _, Some(0), OpFn.Symmetric)
  val multiply  = this has new Item((i, j) => i*j - 2*i - 3*j + 4, None, OpFn.Nonsymmetric)
}

object StrOpFns extends Variants[OpFn[String]] {
  val concat     = this has new Item(_ + _, Some(""), OpFn.Symmetric)
  val interleave = this has new Item((s, t) => (s zip t).map{ case (l,r) => f"$l$r" }.mkString, None, OpFn.Nonsymmetric)
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
