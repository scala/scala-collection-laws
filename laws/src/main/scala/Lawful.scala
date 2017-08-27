package laws

import scala.language.implicitConversions

/** Tags provide a way to select which laws are applicable for a given run.  For instance,
  * if you are testing collections with `Int`s and with `String`s, some tests may be
  * specific to the collection type; in that case you would tag the appropriate laws
  * using a string that helps you distinguish them. (E.g. `Tags("Int")`.)
  *
  * Tags may be either boolean (if present, the value is in a set), or string-valued
  * (represented by a map from tag name to tag value).
  */
case class Tags(flags: Set[String], mappings: Map[String, String]) {
  /** Tests whether any tags are present (either boolean or string-valued) */
  def isEmpty = flags.isEmpty && mappings.isEmpty

  /** Tests whether a tag is present (either boolean or string-valued) */
  def contains(s: String) = (flags contains s) || (mappings contains s)

  /** Sets a boolean tag, overwriting a string-valued tag of the same name if present */
  def set(s: String): Tags =
    if (flags contains s) this
    else if (mappings contains s) new Tags(flags + s, mappings - s)
    else new Tags(flags + s, mappings)

  /** Removes a tag of a given name if present (either boolean or string-valued) */
  def unset(s: String): Tags =
    if (!(flags contains s) && !(mappings contains s)) this
    else new Tags(flags - s, mappings - s)

  override def toString = 
    flags.toList.sorted.mkString("", " ", if (mappings.isEmpty) "" else " ") ++
    mappings.toList.sortBy(_._1).map{ case (k, v) => f"$k->$v" }.mkString(" ")
}
object Tags {
  /** Taggish represents values that can be tags: either a key alone, or a key-value pair (all strings). */
  sealed trait Taggish {}
  final case class StringTag(tag: String) extends Taggish {}
  final case class MapTag(key: String, value: String) extends Taggish {}
  //final case class SelectTag(p: List[String] => Boolean) extends Taggish {}
  /** Implicits contains implicit conversions to values that can be tags. */
  object Implicits {
    implicit def taggishString(s: String) = StringTag(s)
    implicit def taggishKeyValue(kv: (String, String)) = MapTag(kv._1, kv._2)
    //implicit def taggishSelector(p: List[String] => Boolean) = SelectTag(p)
  }

  /** Canonical empty set of tags. (That is, no tags.) */
  val empty = new Tags(Set.empty[String], Map.empty[String, String])

  /** Create a mixed set of boolean and string-valued tags.  First, `import laws.Tags.Implicits._`. */
  def apply(key: Taggish, keys: Taggish*) = {
    val all = key :: keys.toList
    new Tags(
      all.collect{ case StringTag(s) => s      }.toSet,
      all.collect{ case MapTag(k, v) => (k, v) }.toMap
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

  override def toString =
    (if (name.isEmpty) "" else f"# $name\n") +
    code +
    (if (tags.isEmpty) "" else "\n# " + tags.toString) +
    "\n#   from " + Sourced.local(file, line) + ")\n"
}
object Law {
  def apply(code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = new Law(code)(file, line)
  def apply(name: String, code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = new Law(name, code)(file, line)
  def apply(tags: Tags, code: String)(implicit file: sourcecode.File, line: sourcecode.Line) = new Law(tags, code)(file, line)  
}
