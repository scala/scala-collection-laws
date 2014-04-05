package laws

/** A chunk of code incorporating a bunch of asserts.
  * This consists of a preamble, a series of loops (usually fors), and the interior code, plus string flags.
  * The block closings are generated automatically (and are just a closing brace).
  * The interior code is automatically converted to assert statements.
  * The code also can tell you if it thinks a given class can run the code,
  * and keeps track of those classes it thinks are okay (for stats only, not caching).
  */
case class Code(pre: Seq[String], wrap: Seq[String], tests: Seq[(String, Int)], flags: Set[String]) {
  import Code._
  
  private[this] var covernames = List.empty[String]
  def cover(name: String): this.type = { covernames = name :: covernames; this }
  def covered = covernames.nonEmpty
  def coverage = covernames.reverse.toVector
  
  // Generates an error message based on loop context.
  def msg = Seq(
    ("  "*wrap.length) +
    "val message = (lnum: Int) => \"Law line %d;\".format(lnum)" + 
    wrap.map(_.trim).filter(_.startsWith("for (")).map(_.drop(5).
      takeWhile(_.isLetter)).filter(_.length > 0).map(x => """+" %s="+%s.toString""".format(x,x)).
      mkString
  )
  
  def rwrap: Seq[String] = (0 until wrap.length).map("  " * _ + "}").reverse
  
  // The core of the code, properly indented and wrapped in asserts.
  def core = tests.map{ case (line, num) => "  "*wrap.length + s"assert({$line}, message($num))" }
  
  // All the code put together
  def all = pre ++ wrap ++ core ++ rwrap
  
  // Tests whether named methods are supported by the set of classes given.
  // (It's a set to allow for implicit conversion to another class).
  def canRunOn[C <: Class[_]](klasses: Set[C], cflags: Set[String]): Boolean = {
    val available = klasses.flatMap(_.getMethods.filterNot(isStatic)).map(x => dedollar(x.getName))
    val needs = all.flatMap(Parsing.readNeeds).toSet
    val (nflags, yflags) = cflags.partition(_.startsWith("!"))
    (needs subsetOf available) && yflags.subsetOf(flags) && (nflags.map(_.drop(1))&flags).isEmpty
  }
  def canRunOn[C <: Class[_]](klass: C, cflags: Set[String]): Boolean = canRunOn(Set(klass), cflags)
}
object Code {
  // Checks if a method is static; if so, it doesn't count to satisfy a test.
  def isStatic(m: java.lang.reflect.Method) =
    java.lang.reflect.Modifier.isStatic(m.getModifiers)

  // Local reinterpretation of name mangling.  Probably should ask compiler instead.
  val knownDollars = 
    Seq("plus +", "minus -", "times *", "div /", "bslash \\", "colon :", "eq =", "amp &", "tilde ~", "bar |").
    map(_.split(' ') match { case Array(name, sym) => name -> sym })

  // Convert from $plus etc. to symbols.
  def dedollar(s: String, from: Int = 0, prefix: String = ""): String = {
    val i = s.indexOf('$',from)
    if (i < 0 || i+1 >= s.length) prefix + s.substring(from)
    else knownDollars.find{ case (name, _) => s.startsWith(name,i+1) } match {
      case Some((name, sym)) => dedollar(s, i+1+name.length, prefix + s.substring(from,i) + sym)
      case None => dedollar(s, i+1, prefix + s.substring(from, i+1))
    }
  }
}

/** Code that is not tests that knows how to add itself to code to supply a named variable.
  */
case class Supply(name: String, code: String, position: Supply.Pos, also: Option[Supply] = None) {
  import Supply._
  // Adds code in the appropriate place
  def satisfy(lines: Code): Code = {
    val newlines = position match {
      case Outer => lines.copy(pre = code +: lines.pre)
      case Wraps => lines.copy(wrap = code +: lines.wrap.map("  "+_))
    }
    also.map(_.satisfy(newlines)).getOrElse(newlines)
  }
}
object Supply {
  // Places to add code.
  sealed trait Pos
  case object Outer extends Pos
  case object Wraps extends Pos
}
