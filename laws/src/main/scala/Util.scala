/** 
Utils contains various small utility classes and has no dependencies.
*/

package laws

/** N is a mutable counter */
class N(var count: Int = 0) { 
  /** Increment and return new value */
  def ++(): Int = { 
    count += 1
    count
  }
}
object N {
  def apply(count: Int = 0) = new N(count)
}


/** Mu holds an arbitrary mutable value */
class Mu[A](var value: A) {
  def mutf(f: A => A): this.type = {
    value = f(value)
    this
  }
}
object Mu {
  def apply[A](value: A) = new Mu[A](value)
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


/** A typeclass that provides the ability to check the size of something.
  *
  * Intended for use with collections.
  */
trait Sizable[CC] {
  def sizeof(c: CC): Int
}


/** Checks to make sure a set of methods are available */
class MethodChecker(val methods: Set[String]) {
  import scala.reflect._
  import runtime.universe._
  def passes(available: Set[String]) = methods.forall(available)
  def passes(available: MethodChecker) = methods.forall(available.methods)
  def |(that: MethodChecker): MethodChecker = new MethodChecker(methods | that.methods)
}
object MethodChecker {
  import scala.reflect._
  import runtime.universe._

  val empty = new MethodChecker(Set.empty)

  private val anyRefMethods =
    implicitly[TypeTag[AnyRef]].tpe.members.
      collect{ case x if x.isMethod => x.asMethod }.
      filter(_.isPublic).
      map(_.name.decodedName.toString).
      toSet

  private val ignoredMethods = Set(
    "$init$",
    "canEqual",
    "clone",
    "par",
    "seq",
    // General methods from functions (usually the default implementations are used)
    "andThen",
    "compose",
    // General methods from partial functions (usually the default implementations are used)
    "applyOrElse",
    "lift",
    "orElse",
    "runWith"
  )

  private val assumedMethods = Set(
    "filter",
    "flatMap",
    "map"
  )

  def from[C: TypeTag]: MethodChecker = {
    val tp = implicitly[TypeTag[C]].tpe
    val meths = tp.members.collect{ case x if x.isMethod => x.asMethod }.filter(_.isPublic)
    new MethodChecker(
      meths.map(_.name.decodedName.toString).filterNot(_.contains("$default")).toSet 
      -- anyRefMethods
      -- ignoredMethods
      ++ assumedMethods
    )
  }
}


/** Builds and caches an array of values.  (Not thread-safe.)
  *
  * It just acts like a buffer that fixes its contents once in use.
  *
  * It is useful for cases where you want to list names that are automatically picked up by the `sourcecode` package.
  */
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


/** Utility methods to perform file I/O in the context of code generation, where there
  * is some name-mangling, and you don't want to write the file if you haven't changed
  * the contents.
  */
object FileIO {
  /** Removes all the test files in a particular directory; throws an exception if anything goes wrong. */
  def desource(dir: java.io.File): Unit = {
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
    val myLines = collection.immutable.ArraySeq.from(content.linesIterator)
    val different =
      if (target.exists) {
        val src = scala.io.Source.fromFile(target)
        try {
          val lines = src.getLines().toVector.map(trimRight)
          lines != myLines.map(trimRight)
        }
        finally src.close
      }
      else true
    if (different) java.nio.file.Files.write(target.toPath, java.util.Arrays.asList(myLines: _*))
    different
  }
}
