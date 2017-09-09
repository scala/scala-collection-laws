package laws

import scala.language.implicitConversions

/** Information that you can use to filter tests */
trait TestInfo {
  def hasZero: Boolean
  def isSymOp: Boolean
  def flags: Set[String]
  final def runtimeElt: java.lang.Class[_] = {
    val r = runtimeElt
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
}

/** Tags provide a way to select which laws are applicable for a given run.  For instance,
  * if you are testing collections with `Int`s and with `String`s, some tests may be
  * specific to the collection type; in that case you would tag the appropriate laws
  * using a string that helps you distinguish them. (E.g. `Tags("Int")`.)
  *
  * Tags may be either boolean (if present, the value is in a set), or string-valued
  * (represented by a map from tag name to tag value).
  */
case class Tags(positive: Set[String], negative: Set[String], select: Vector[TestInfo => Boolean]) {
  /** Tests whether any tags are present (either boolean or string-valued) */
  val isEmpty = positive.isEmpty && negative.isEmpty && select.isEmpty

  def check(t: TestInfo) =
    positive.forall(t.flags) && !negative.exists(t.flags) && select.forall(p => p(t))

  /** Sets a boolean tag that must be present */
  def need(s: String): Tags =
    if (positive contains s) this
    else if (negative contains s) new Tags(positive + s, negative - s, select)
    else new Tags(positive + s, negative, select)

  /** Requires that a particular tag be absent */
  def shun(s: String): Tags =
    if (negative contains s) this
    else if (positive contains s) new Tags(positive - s, negative + s, select)
    else new Tags(positive, negative + s, select)

  def filter(p: TestInfo => Boolean): Tags = new Tags(positive, negative, select :+ p)

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
  final case class PosTag(tag: String) extends Taggish {}
  final case class NegTag(tag: String) extends Taggish {}
  final case class SelectTag(p: TestInfo => Boolean) extends Taggish {}
  //final case class SelectTag(p: List[String] => Boolean) extends Taggish {}
  /** Implicits contains implicit conversions to values that can be tags. */
  object Implicits {
    implicit class StringIsTaggish(s: String) {
      def y: PosTag = PosTag(s)
      def n: NegTag = NegTag(s)
    }
    implicit def stringIsPositiveByDefault(s: String): PosTag = PosTag(s)
    implicit def select(p: TestInfo => Boolean): SelectTag = SelectTag(p)
  }

  /** Canonical empty set of tags. (That is, no tags.) */
  val empty = new Tags(Set.empty[String], Set.empty[String], Vector.empty[TestInfo => Boolean])

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

case class Law(name: String, tags: Tags, code: String, disabled: Boolean = false)(implicit file: sourcecode.File, line: sourcecode.Line) {
  def this(code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = this("", Tags.empty, code)(file, line)
  def this(name: String, code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = this(name, Tags.empty, code)(file, line)
  def this(tags: Tags, code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = this("", tags, code)(file, line)

  private[this] def findMyMethods: Either[FormatErr, Set[String]] = {
    val b = Array.newBuilder[String]
    var i = 0
    while (i >= 0 && i < code.length) {
      i = code.indexOf('`', i)
      if (i >= 0) {
        val j = code.indexOf('`', i+1)
        if (j > i+1) b += code.substring(i+1, j)
        else return Left(FormatErr("Unclosed method quotes", code, i, code.substring(i)))
        i = j
      }
    }
    Right(b.result.toSet)
  }
  /** Methods in the law that are backtick-quoted, indicating that the collection should only be used if it has those methods */
  val methods = findMyMethods
  val check = findMyMethods.fold(_ => MethodChecker.missing, s => new MethodChecker(s))

  val lineNumber = line.value

  override def toString =
    (if (name.isEmpty) "" else f"// $name\n") +
    code +
    (if (tags.isEmpty) "" else "\n// # " + tags.toString) +
    "\n// @ " + Sourced.local(file, line) + "\n"
}
object Law {
  def apply(code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = new Law(code)(file, line)
  def apply(name: String, code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = new Law(name, code)(file, line)
  def apply(tags: Tags, code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = new Law(tags, code)(file, line)  
}
