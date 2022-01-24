package laws

import scala.language.implicitConversions
import scala.language.higherKinds

import scala.util._

/** The collection to be tested: this provides all elements and collections
  * and mappings thereon which are available inside the tests.
  *
  * Tests are immutable; if you want a different set of values,
  * create a new test.  You can use the `renumber`, `reoperate`, and
  * `reinstance` methods to create a new test with the corresponding
  * values changed.
  *
  * Subclasses are intended to implement tests for _every_ valid
  * law, so a `relaw` method, to switch the law line number,
  * is provided as well.  This method is not safe in that the
  * line number could be invalid.  The `run` method will then
  * throw an exception.
  */
abstract class Test[A, B, CC, T <: Test[A, B, CC, T]](
  val num: Numbers,
  val ops: Ops[A, B],
  val instance: Instance[A, CC],
  val lawLine: Int
)(implicit file: sourcecode.File, line: sourcecode.Line, nm: sourcecode.Name) 
extends Sourced
with Named 
with TestInfo {
  /** Arbitrary element of the type in the collection */
  def a: A = instance.values.a

  /** An arbitrary element of a type not in the basic collection.  Should match g(a). */
  lazy val b: B = ops.values.g.fn(instance.values.a)

  /** Some function that preserves the type of the elements */
  def f: A => A = ops.f

  /** Some function that changes the type of the elements.  Note that g(a) == b should be true. */
  def g: A => B = ops.g

  /** A positive number that is supposedly no bigger than the length of `x` */
  def n: Int = num.n

  /** A positive number that may be bigger than the length of `x` */
  def nn: Int = num.nn

  /** A positive number that is no bigger than the length of `y` */
  def m: Int = num.m

  /** A positive number that may be bigger than the length of `y` **/
  def mm: Int = num.mm

  /** Some integer.  May be positive or negative.  Could really be anything. */
  def r: Int = num.r

  /** A binary operation on the collection elements.
    *
    * If no such operation exists, leave it as `Coll.noOp` and tests requiring this operator will be skipped.
    */
  def op: (A, A) => A = ops.op

  /** A predicate on the type of the elements */
  def p: A => Boolean = ops.p

  /** A partial function that preserves the type of the elements */
  def pf: PartialFunction[A, A] = ops.pf

  /** A specific collection of some size */
  def x: CC = instance.x

  /** The known size of the collection `x` */
  def xsize: Int = instance.xsize

  /** Another specific collection, not necessarily the same as `x` */
  def y: CC = instance.y

  /** The known size of collection `y` */
  def ysize: Int = instance.ysize

  /** Element of type `A` that is a zero with respect to `op`, if `op` exists and a zero exists */
  def zero: A = ops.z

  /** Converts a value to a characteristic integer.  (Default is just hashcode.) */
  def intFrom(a: A): Int = a.##

  /** Builds a fresh instance of the collection type being tested using an array as a source */
  def collectionFrom(aa: Array[A]): CC = instance.newInstanceFrom(aa)

  def flags: Set[Flag] = instance.flags

  def oper: Ops[_, _] = ops

  def inst: Instance[_, _] = instance

  protected def boxedRuntime = a.getClass

  def runtimeColl = x.getClass

  def name = nm.value.toString

  val bitset_f = (i: Int) => i/2 + 999

  override lazy val toString =
    nm.value.toString + " @ " + source +
    f"\n  $num\n" +
    instance.toString.split("\n").map("  " + _).mkString("", "\n", "\n") +
    ops.toString.split("\n").map("  " + _).mkString("", "\n", "\n")

  def tryE[A](f: => A): Either[Throwable, A] = try { Right(f) } catch { case e: Throwable => Left(e) }

  def tryO[A](f: => A): Option[A] = try { Some(f) } catch { case _: Throwable => None }

  def renumber(numb: Numbers): T

  def reinstance(inst: Instance[A, CC]): T

  def reoperate(ops: Ops[A, B]): T

  def relaw(lln: Int): T

  /** This is the actual test which runs a law and returns `Some(true)` if it
    * passes with these parameters, `Some(false)` if it fails, or `None` if
    * the law is not valid for this collection.  It does NOT keep track of whether
    * the law has been run or not.
    */
  def runLaw(n: Int): Option[Boolean]

  /** Runs on the default law given by the `lawLine` parameter.  Throws an exception if the law number doesn't exist. */
  def run: Boolean = runLaw(lawLine).get
}
object Test {
  /** Information about a test that has passed its law for every combination of parameters */
  case class Pass(law: Law, iterations: Long, time: Double) {}

  /** Information about a test that has failed its law for some combination of parameters */
  case class Fail(law: Law, outcome: Outcome, test: Option[Test[_, _, _, _]], exception: Option[Throwable]) {
    override def toString = {
      val testString = if (outcome.hasTest && test.isDefined) "Some(...)" else test.toString
      val exceptionString = if (outcome.hasException && exception.isDefined) "Some(...)" else exception.map(e => Outcome.partialStackTrace(e)).toString
      f"Fail($law, $outcome, ${}, $testString, $exceptionString}"
    }
  }

  /** Information about the test results for all relevant laws on a particular collection */
  case class Tested(succeeded: Map[Int, Pass], failed: Map[Int, Fail], missed: Set[Int], methods: Set[String]) {
    /** For each method name, lists the line numbers of all laws that passed all attempted values (first list)
      * and the line numbers of laws that failed some value (second list)
      */
    def findMethodUsage: Map[String, (List[Int], List[Int])] = {
      val usage = methods.toArray.map{ m => (m, (Mu(List.empty[Int]), Mu(List.empty[Int]))) }.toMap
      for {
        (_, Pass(law, _, _)) <- succeeded
        m <- law.methods.getOrElse(Set.empty[String])
      } usage.get(m).foreach(_._1.mutf(law.lineNumber :: _))
      for {
        (_, Fail(law, _, _, _)) <- failed
        m <- law.methods.getOrElse(Set.empty[String])
      } usage.get(m).foreach(_._2.mutf(law.lineNumber :: _))
      usage.map{ case (m, (mu1, mu2)) => (m, (mu1.value.sorted, mu2.value.sorted)) }
    }
  }

  /** A generic companion to test classes.
    *
    * Some of the methods are abstract and are filled in by the code generator.
    */
  trait Companion extends Named {
    /** The laws tested by this (kind of) test */
    def obeys: Map[Int, Law]

    /** The methods possessed by the collection in this kind of test */
    def methods: Set[String]

    /** Keeps track of which laws were actually run */
    lazy val ran = new collection.mutable.HashSet[Int]

    /** Runs a single test */
    def run(lln: Int): Either[Outcome, Long]

    /** Runs the tests on all the laws */
    def runAll(): Tested = {
      val good = new collection.mutable.HashMap[Int, Pass]
      val bad = new collection.mutable.HashMap[Int, Fail]
      val missed = new collection.mutable.HashSet[Int]
      obeys.foreach{ case (lln, law) =>
        val before = System.nanoTime
        val result = Try{ run(lln) }
        val elapsed = 1e-9*(System.nanoTime - before)
        result match {
          case Failure(e) => bad(lln) = Fail(law, Outcome.Error(e), None, Some(e))
          case Success(x) => x match {
            case Right(n) => if (n > 0) good(lln) = Pass(law, n, elapsed) else missed += lln
            case Left(o) => o match {
              case Outcome.Missing(n)                    => missed += lln
              case Outcome.Failed(t: Test[_, _, _, _])   => bad(lln) = Fail(law, o, Some(t), None)
              case Outcome.Threw(t: Test[_, _, _, _], e) => bad(lln) = Fail(law, o, Some(t), Some(e))
              case Outcome.Error(e)                      => bad(lln) = Fail(law, o, None,    Some(e))
              case _                                     => bad(lln) = Fail(law, o, None,    None)
            }
          }
        }
      }
      Tested(good.toMap, bad.toMap, missed.toSet, methods)
    }
  }

  /** Abstracts over traversing over a collection so that we can
  use different collections libraries and be less sensitive to
  which one we're comparing to which (all should convert to this) */
  abstract class Once[A] {
    def step[U](f: A => U): Boolean

    final def foreach[U](f: A => U): Unit =
      while(step(f)) {}

    final def forallWith[B](that: Once[B])(p: (A, B) => Boolean): Boolean = {
      var aok, bok, ok = true
      while (ok) {
        var a: A = null.asInstanceOf[A]
        var b: B = null.asInstanceOf[B]
        aok = this.step(a = _)
        bok = that.step(b = _)
        ok = aok && bok && p(a, b)
      }
      !aok && !bok && !ok
    }
  }
  object Once {
    def from(string: String): Once[Char] = new Once[Char] {
      private[this] var i = 0
      def step[U](f: Char => U) = (i < string.length) && { f(string(i)); i += 1; true }
    }

    def from[A](array: Array[A]): Once[A] = new Once[A] {
      private[this] var i = 0
      def step[U](f: A => U): Boolean = (i < array.length) && { f(array(i)); i += 1; true }
    }

    def from[A](iterator: Iterator[A]): Once[A] = new Once[A] {
      def step[U](f: A => U): Boolean = iterator.hasNext && { f(iterator.next); true }
    }
    def from[A](iterable: Iterable[A]): Once[A] = from(iterable.iterator)

    trait LowPriorityConversions {
      implicit def onceViaAccumulator[A, CC[X] <: collection.mutable.Seq[X]](acc: scala.jdk.Accumulator[A, CC, _]): Once[A] =
        Once from acc.iterator      
    }
    object Conversions extends LowPriorityConversions {
      implicit def onceViaString(string: String): Once[Char] = Once from string
      implicit def onceViaArray[A](array: Array[A]): Once[A] = Once from array
      implicit def onceViaIterableOnce[A, CC[A] <: collection.IterableOnce[A]](me: CC[A]): Once[A] =
        me match {
          case iterator: Iterator[_] => Once from iterator.asInstanceOf[Iterator[A]]
          case iterable: Iterable[_] => Once from iterable.asInstanceOf[Iterable[A]]
          case _                     => Once from me.iterator.to(List)
        }
      implicit def onceViaIterableTuple[K, V, CC[K, V] <: collection.Iterable[(K, V)]](me: CC[K, V]): Once[(K, V)] = Once from me
    }
  }


  /** Tests whether the compiler believes the left-hand and right-hand types are the same */
  implicit class SameCompilerType[A](me: A) {
    def sameType[B](you: B)(implicit ev: A =:= B) = true
  }

  /** Tests whether two collections have the same elements in the same order */
  implicit class EqualInOrder[A, CC](me: CC)(implicit onceCC: CC => Once[A]) {
    def sameAs[DD](you: DD)(implicit onceDD: DD => Once[A]) = 
      onceCC(me).forallWith(onceDD(you))(_ == _)
  }

  /** Tests whether two collections have the same number of each element. */
  implicit class EqualInCount[A, CC](me: CC)(implicit onceCC: CC => Once[A]) {
    def sameAs[DD](you: DD)(implicit onceDD: DD => Once[A]) = {
      val meM, youM = collection.mutable.HashMap.empty[A, N]
      onceCC(me).foreach(a => meM.getOrElseUpdate(a, new N).++)
      onceDD(you).foreach(a => youM.getOrElseUpdate(a, new N).++)
      meM.forall{ case (a, n) => youM.get(a).exists(_.count == n.count) } &&
      youM.forall{ case (a, n) => meM contains a }
    }
  }

  /** Tests whether the right-hand collection has every element the left-hand collection does (it may have more). */
  implicit class SubsetInCount[A, CC](me: CC)(implicit onceCC: CC => Once[A]) {
    def samePieces[DD](you: DD)(implicit onceDD: DD => Once[A]) = {
      val meM, youM = collection.mutable.HashMap.empty[A, N]
      onceCC(me).foreach(a => meM.getOrElseUpdate(a, new N).++)
      onceDD(you).foreach(a => youM.getOrElseUpdate(a, new N).++)
      meM.forall{ case (a, n) => youM.get(a).exists(_.count == n.count) } &&
      youM.forall{ case (a, n) => meM contains a }
    }
    def partOf[DD](you: DD)(implicit onceDD: DD => Once[A]) = {
      val meM, youM = collection.mutable.HashMap.empty[A, N]
      onceCC(me).foreach(a => meM.getOrElseUpdate(a, new N).++)
      onceDD(you).foreach(a => youM.getOrElseUpdate(a, new N).++)
      meM.forall{ case (a, n) => youM.get(a).exists(_.count >= n.count) }
    }
  }

  /** Shorthand short-circuiting logic operators */
  implicit class Logic(lhs: Boolean) {
    def implies(rhs: => Boolean) = !lhs || rhs
    def impliedBy(rhs: => Boolean) = lhs || !rhs
  }
}


/////////////////////////////////////////
// Selection of element type for tests //
/////////////////////////////////////////

/** Tests that use `Int` values extend this class. */
abstract class IntTest[CC, T <: IntTest[CC, T]](
  numb: Numbers, oper: Ops[Int, Long], inst: Instance[Int, CC], lln: Int
)(
  implicit file: sourcecode.File, line: sourcecode.Line, name: sourcecode.Name
)
extends Test[Int, Long, CC, T](numb, oper, inst, lln)(file, line, name) {
  type A = Int
  type B = Long
  type Inst = Instance[Int, CC]
  type Oper = Ops[Int, Long]
  def maxOf(a: Int, aa: Int) = a max aa
  def minOf(a: Int, aa: Int) = a min aa
  override def intFrom(a: Int) = a
}

/** Tests that use `String` values extend this class. */
abstract class StrTest[CC, T <: StrTest[CC, T]](
  numb: Numbers, oper: Ops[String, Option[String]], inst: Instance[String, CC], lln: Int
)(
  implicit file: sourcecode.File, line: sourcecode.Line, name: sourcecode.Name
)
extends Test[String, Option[String], CC, T](numb, oper, inst, lln)(file, line, name) {
  type A = String
  type B = Option[String]
  type Inst = Instance[String, CC]
  type Oper = Ops[String, Option[String]]
  def maxOf(a: String, aa: String) = { val o = implicitly[Ordering[String]]; o.max(a, aa) }
  def minOf(a: String, aa: String) = { val o = implicitly[Ordering[String]]; o.min(a, aa) }
  override def intFrom(s: String) = s.length
}

/** Tests (for maps) that use `(Long, String)` values extend this class. */
abstract class LongStrTest[CC, T <: LongStrTest[CC, T]](
  numb: Numbers, oper: Ops[(Long, String), (String, Long)], inst: Instance[(Long, String), CC], lln: Int
)(
  implicit file: sourcecode.File, line: sourcecode.Line, name: sourcecode.Name
)
extends Test[(Long, String), (String, Long), CC, T](numb, oper, inst, lln)(file, line, name) {
  type K = Long
  type V = String
  type A = (K, V)
  type B = (V, K)
  type Inst = Instance[(K, V), CC]
  type Oper = Ops[(K, V), (V, K)]

  def maxOf(a: (Long, String), aa: (Long, String)) = { val o = implicitly[Ordering[(Long, String)]]; o.max(a, aa) }
  def minOf(a: (Long, String), aa: (Long, String)) = { val o = implicitly[Ordering[(Long, String)]]; o.min(a, aa) }
  override def intFrom(kv: (Long, String)) = kv._1.toInt
}

/** Tests (for maps) that use `(String, Long)` values extend this class. */
abstract class StrLongTest[CC, T <: StrLongTest[CC, T]](
  numb: Numbers, oper: Ops[(String, Long), (Long, String)], inst: Instance[(String, Long), CC], lln: Int
)(
  implicit file: sourcecode.File, line: sourcecode.Line, name: sourcecode.Name
)
extends Test[(String, Long), (Long, String), CC, T](numb, oper, inst, lln)(file, line, name) {
  type K = String
  type V = Long
  type A = (K, V)
  type B = (V, K)
  type Inst = Instance[(K, V), CC]
  type Oper = Ops[(K, V), (V, K)]

  def maxOf(a: (String, Long), aa: (String, Long)) = { val o = implicitly[Ordering[(String, Long)]]; o.max(a, aa) }
  def minOf(a: (String, Long), aa: (String, Long)) = { val o = implicitly[Ordering[(String, Long)]]; o.min(a, aa) }
  override def intFrom(kv: (String, Long)) = kv._2.toInt
}

