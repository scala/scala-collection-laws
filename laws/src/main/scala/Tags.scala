package laws

import scala.language.implicitConversions

/** A tag that can be attached to a collection or law to categorize its properties */
final class Tag()(implicit val nm: sourcecode.Name)
extends Ordered[Tag] { 
  override val toString = nm.value.toString
  override def compare(that: Tag) = toString.compareTo(that.toString)
  override def equals(that: Any) = that match {
    case t: Tag => this.toString == that.toString
    case _      => false
  }
  override val hashCode = toString.hashCode
}
object Tag {
  def T(implicit nm: sourcecode.Name) = new Tag()(nm)

  // List these one by one to be sure the tag picks up the right name!
  val ARR = T
  val INT = T
  val MAP = T
  val SEQ = T
  val SET = T
  val STR = T
}

/** Information that you can use to filter tests (note: only flags are used for compile-time generation) */
trait TestInfo {
  def num: Numbers
  def oper: Ops[_, _]
  def inst: Instance[_, _]
  final def runtimeElt: java.lang.Class[_] = {
    val r = boxedRuntime
    if      (r == classOf[java.lang.Integer])       classOf[Int]
    else if (r == classOf[java.lang.Long])          classOf[Long]
    else if (r == classOf[java.lang.Double])        classOf[Double]
    else if (r == classOf[java.lang.Float])         classOf[Float]
    else if (r == classOf[java.lang.Character])     classOf[Char]
    else if (r == classOf[java.lang.Byte])          classOf[Byte]
    else if (r == classOf[java.lang.Short])         classOf[Short]
    else if (r == classOf[scala.runtime.BoxedUnit]) classOf[Unit]
    else                                            r
  }
  def boxedRuntime: java.lang.Class[_]
  def runtimeColl: java.lang.Class[_]

  def skipMissingZero    = if (oper.values.op.zero.isDefined)            None else Some(Outcome.Skip.op)
  def skipNonassociative = if (oper.values.op.assoc == OpFn.Associative) None else Some(Outcome.Skip.op)
}

/** Tags provide a way to select which laws are applicable for a given run.  For instance,
  * if you are testing collections with `Int`s and with `String`s, some tests may be
  * specific to the collection type; in that case you would tag the appropriate laws
  * using a string that helps you distinguish them. (E.g. `Tags("Int")`.)
  *
  * Additional restriction of the set may be achieved by including tests in `select`.  All
  * such tests must pass for a particular set of parameters to be included in a test.
  */
case class Tags(positive: Set[Tag], negative: Set[Tag], select: Vector[TestInfo => Option[Outcome.Skip]]) {
  /** Tests whether any tags are present (either boolean or string-valued) */
  val isEmpty = positive.isEmpty && negative.isEmpty && select.isEmpty

  /** Checks whether a certain set of flags is compatible for code generation (compile-time compatible) */
  def compatible(flags: Set[Tag]) =
    positive.forall(flags contains _) && !flags.exists(negative contains _)

  /** Checks whether a certain choice of parameters is suitable for testing at runtime */
  def validate(t: TestInfo): Option[Outcome.Skip] = select.iterator.map(p => p(t)).find(_.isDefined).flatten

  /** Sets a boolean tag that must be present */
  def need(t: Tag): Tags =
    if (positive contains t) this
    else if (negative contains t) new Tags(positive + t, negative - t, select)
    else new Tags(positive + t, negative, select)

  /** Requires that a particular tag be absent */
  def shun(t: Tag): Tags =
    if (negative contains t) this
    else if (positive contains t) new Tags(positive - t, negative + t, select)
    else new Tags(positive, negative + t, select)

  /** Adds an extra selector that checks test info */
  def filter(p: TestInfo => Option[Outcome.Skip]): Tags = new Tags(positive, negative, select :+ p)

  override lazy val toString = {
    val named = positive.toList.sorted ::: negative.toList.map("!" + _).sorted
    val pred  = select.length match {
      case 0 => ""
      case 1 => "(1 filter)"
      case n => f"($n filters)"
    }
    (if (pred.nonEmpty) named.toVector :+ pred else named.toVector).mkString(" ")
  }
}
object Tags {
  /** Taggish represents values that can be tags: either a key alone, or a key-value pair (all strings). */
  sealed trait Taggish {}
  final case class PosTag(tag: Tag) extends Taggish {}
  final case class NegTag(tag: Tag) extends Taggish {}
  final case class SelectTag(p: TestInfo => Option[Outcome.Skip]) extends Taggish {}
  //final case class SelectTag(p: List[String] => Boolean) extends Taggish {}
  /** Implicits contains implicit conversions to values that can be tags. */
  object Implicits {
    implicit class TagIsTaggish(t: Tag) {
      def y: PosTag  = PosTag(t)
      def n: NegTag  = NegTag(t)
      def ! : NegTag = NegTag(t)
    }
    implicit def tagIsPositiveByDefault(t: Tag): PosTag = PosTag(t)
    implicit def select(p: TestInfo => Option[Outcome.Skip]): SelectTag = SelectTag(p)
  }

  /** Canonical empty set of tags. (That is, no tags.) */
  val empty = new Tags(Set.empty[Tag], Set.empty[Tag], Vector.empty[TestInfo => Option[Outcome.Skip]])

  /** Create a mixed set of boolean and predicate tags.
    *
    * First, `import laws.Tags.Implicits._`.  Then use `"seq".y, "set".n, select(_.hasZero)` to set,
    * in this example, a tag that must be present, mustn't be present, and a test that must pass, respectively.
    */
  def apply(key: Taggish, keys: Taggish*) = {
    val all = key :: keys.toList
    val positive = all.collect{ case PosTag(s)    => s }.toSet
    new Tags(
      positive,
      all.collect{ case NegTag(s)    => s }.toSet &~ positive,
      all.collect{ case SelectTag(p) => p }.toVector
    )
  }
}
