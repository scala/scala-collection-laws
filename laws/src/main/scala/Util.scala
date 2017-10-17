package laws

/** FormatErr represents an error in the formatting of a collection law.  Presently,
  * the only thing that can go wrong is an unclosed method name (the checked methods
  * should be enclosed in backticks).
  */
case class FormatErr(description: String, context: String, position: Int, focus: String) {
  override def toString = f"$description.  At $position found $focus.  In $context"
}

/** Trait that captures the idea of having a source from which one is generated.
  */
trait Sourced {
  final def source(implicit file: sourcecode.File, line: sourcecode.Line) = Sourced.local(file, line)
}
object Sourced {
  /** A text description of the file and line some source came from */
  def local(file: sourcecode.File, line: sourcecode.Line): String =
    (new java.io.File(file.value)).getName + ", line " + line.value

  /** A text description of file and line using implicit values in scope (or the current line) */
  def implicitly(implicit file: sourcecode.File, line: sourcecode.Line): String =
    local(file, line)
}

/** Trait that captures the idea of having a known variable name */
trait Named {
  def name: String
}

/** Caches a computation that we expect to generate something immutable (so a cache is fine) */
final class CachedFn0[A](val underlying: () => A) extends (() => A) {
  private lazy val cache = underlying()
  def apply(): A = cache
}

/** Checks to make sure a set of methods are available */
class MethodChecker(methods: Set[String]) {
  import scala.reflect._
  import runtime.universe._
  def passes(available: Set[String]) = methods.forall(available)
  def apply[C: TypeTag](c: C): Boolean = passes(MethodChecker.list(c))
}
object MethodChecker {
  import scala.reflect._
  import runtime.universe._

  val empty = new MethodChecker(Set.empty)

  def list[C: TypeTag](c: C): Set[String] = {
    val tp = implicitly[TypeTag[C]].tpe
    val meths = tp.members.collect{ case x if x.isMethod => x.asMethod }
    meths.map(_.name.decodedName.toString).toSet
  }
}

object FileIO {
  /** Removes all the test files in a particular directory; throws an exception if anything goes wrong. */
  def desource(dir: java.io.File) {
    val oops =
      dir.listFiles.
        filter(f => f.getName.startsWith("Test") && f.getName.endsWith(".scala")).
        find(f => !f.delete())
    oops.foreach{ f =>
      println(s"Failed to delete $f")
      throw new Exception(s"Could not remove source of $f")
    }
  }

  private[this] def trimRight(s: String): String = {
    var i = s.length - 1
    while (i >= 0 && java.lang.Character.isWhitespace(s.charAt(i))) i -= 1;
    if (i+1 < s.length) s.substring(0, i+1) else s
  }

  /** Replaces a text file with new text if the new text is different (trailing whitespace ignored).
    * Returns true if replacement occured, false if not, and throws an exception if any I/O failed.
    */
  def apply(target: java.io.File, content: String): Boolean = {
    val myLines: Array[String] = content.lines.toArray
    val different =
      if (target.exists) {
        val src = scala.io.Source.fromFile(target)
        try {
          val lines = src.getLines.toVector.map(trimRight)
          lines != (myLines: Seq[String]).map(trimRight)
        }
        finally src.close        
      }
      else true
    if (different) java.nio.file.Files.write(target.toPath, java.util.Arrays.asList(myLines: _*))
    different
  }
}
