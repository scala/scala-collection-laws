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
class OpFn[X](val ofn: (X, X) => X, val zero: Option[X])(implicit file: sourcecode.File, line: sourcecode.Line, nm: sourcecode.Name)
extends Named {
  def name = nm.value.toString
  override val toString = nm.value.toString + " @ " + Sourced.implicitly
  override def equals(that: Any) = that match {
    case x: OpFn[_] => toString == x.toString
    case _          => false
  }
  override val hashCode = scala.util.hashing.MurmurHash3.stringHash(toString)
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
final class Active[A, B] private (
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
  val z: A = { _opzCount += 1; op0.zero.get }

  val hasZ: Boolean = op0.zero.isDefined

  def fCount = _fCount
  def gCount = _gCount
  def opzCount = _opzCount
  def pCount = _pCount
  def pfCount = _pfCount
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
    case a: Active[_, _] => (f0 == a.f0) && (g0 == a.g0) && (op0 == a.op0) && (p0 == a.p0) && (pf0 == a.pf0)
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
    paddedParts.mkString("Active\n  ", "\n  ", "")
  }
}
object Active {
  def apply[A, B](f: A ===> A, g: A ===> B, op: OpFn[A], p: A ===> Boolean, pf: ParFn[A]) =
    new Active(f, g, op, p, pf)
}


/////////////////////////////////////////
// Elaboration of particular behaviors //
/////////////////////////////////////////

trait Variants[A] {
  type Item = A
  def all: Array[A]
}

object IntFns extends Variants[Int ===> Int] {
  val plusOne   = new Item(_ + 1)
  val quadratic = new Item(i => i*i - 3*i + 1)
  val all = Array(plusOne, quadratic)
}

object StrFns extends Variants[String ===> String] {
  val upper = new Item(_.toUpperCase)
  val fishy = new Item(s => f"<$s-<")
  val all = Array(upper, fishy)
}

object IntToLongs extends Variants[Int ===> Long] {
  val bit33 = new Item(i => 0x200000000L | i)
  val cast  = new Item(i => i.toLong)
  val all = Array(bit33, cast)
}

object StringToOption extends Variants[String ===> Option[String]] {
  val natural = new Item(s => Option(s).filter(_.length > 0))
  val letter  = new Item(s => Option(s.filter(_.isLetter)).filter(_.length > 0))
  val all = Array(natural, letter)
}

object IntOps extends Variants[OpFn[Int]] {
  val summation = new Item(_ + _, Some(0))
  val multiply  = new Item((i, j) => i*j - 2*i - 3*j + 4, None)
  val all = Array(summation, multiply)
}

object StrOps extends Variants[OpFn[String]] {
  val concat     = new Item(_ + _, Some(""))
  val interleave = new Item((s, t) => (s zip t).map{ case (l,r) => f"$l$r" }.mkString, None)
  val all = Array(concat, interleave)
}

object IntPreds extends Variants[Int ===> Boolean] {
  val mod3   = new Item(i => (i%3) == 0)
  val always = new Item(_ => true)
  val never  = new Item(_ => false)
  val all = Array(mod3, always, never)
}

object StrPreds extends Variants[String ===> Boolean] {
  val increasing = new Item(s => s.length < 2 || s(0) <= s(s.length-1))
  val always     = new Item(_ => true)
  val never      = new Item(_ => false)
  val all = Array(increasing, always, never)
}

object IntParts extends Variants[ParFn[Int]] {
  val halfEven    = new Item({ case x if (x % 2) == 0 => x / 2 })
  val identical   = new Item({ case x => x })
  val uninhabited = new Item(Function.unlift((i: Int) => None: Option[Int]))
  val all = Array(halfEven, identical, uninhabited)
}

object StrParts extends Variants[ParFn[String]] {
  val oddMirror   = new Item({ case x if (x.length % 2) == 1 => x.reverse })
  val identical   = new Item({ case x => x })
  val uninhabited = new Item(Function.unlift((s: String) => None: Option[String]))
  val all = Array(oddMirror, identical, uninhabited)
}
