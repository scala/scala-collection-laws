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
  def apply[C: TypeTag](c: C): Boolean = { val m = MethodChecker.list(c); methods.forall(m) }
}
object MethodChecker {
  import scala.reflect._
  import runtime.universe._

  val missing = new MethodChecker(Set.empty) { 
    override def apply[C: TypeTag](c: C) = false
  }

  def list[C: TypeTag](c: C): Set[String] = {
    val tp = implicitly[TypeTag[C]].tpe
    val meths = tp.members.collect{ case x if x.isMethod => x.asMethod }
    meths.map(_.name.decodedName.toString).toSet
  }
}

/** Indicates that there is some set of parameters to be explored */
trait Exploratory[A] { self =>
  def sizes: Array[Int]

  final def explore: Explore = new Explore(sizes)

  protected def validate(ixs: Array[Int]): Boolean =
    if (ixs.length != sizes.length) false
    else {
      var i = 0
      while (i < ixs.length) {
        if (ixs(i) < 0 || ixs(i) >= sizes(i)) return false
        i += 1
      }
      true
    }

  def lookup(ixs: Array[Int]): Option[A]
  final def lookup(e: Explore): Option[A] = e.current.flatMap{ ixs => lookup(ixs) }

  def map[B](f: A => B): Exploratory[B] = new Exploratory[B] {
    def sizes = self.sizes
    def lookup(ixs: Array[Int]) = self.lookup(ixs).map(f)
  }
}
