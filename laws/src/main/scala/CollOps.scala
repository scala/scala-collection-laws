package laws


///////////////////////////////////////
// Selection of base collection type //
///////////////////////////////////////

abstract class IntColl[CC](
  values: Values, coll: Provider[Int, CC], act: Active[Int, Long]
)(
  implicit file: sourcecode.File, line: sourcecode.Line
)
extends Coll[Int, Long, CC](values, coll, act)(file, line) {}

abstract class StrColl[CC](
  values: Values, coll: Provider[String, CC], act: Active[String, Option[String]]
)(
  implicit file: sourcecode.File, line: sourcecode.Line
)
extends Coll[String, Option[String], CC](values, coll, act)(file, line) {}


////////////////////////////////////////////////
// Selection of particular types of behaviors //
////////////////////////////////////////////////

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
