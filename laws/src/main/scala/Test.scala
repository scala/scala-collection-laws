package laws

import scala.util._

/** The collection to be tested: this provides all elements and collections
  * and mappings thereon which are available inside the tests.
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

  def flags: Set[Tag] = instance.flags

  def oper: Ops[_, _] = ops

  def inst: Instance[_, _] = instance

  def boxedRuntime = a.getClass

  def runtimeColl = x.getClass

  def name = nm.value.toString

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
  case class Fail(law: Law, outcome: Outcome, test: Option[Test[_, _, _, _]], exception: Option[Throwable]) {}

  case class Tested(succeeded: Map[Int, Long], failed: Map[Int, Fail], missed: Set[Int]) {}

  trait Companion extends Named {
    /** The laws tested by this (kind of) test */
    def obeys: Map[Int, Law]

    /** Keeps track of which laws were actually run */
    lazy val ran = new collection.mutable.HashSet[Int]

    /** Runs a single test */
    def run(lln: Int): Either[Outcome, Long]

    /** Runs the tests on all the laws */
    def runAll(): Tested = {
      val good = new collection.mutable.HashMap[Int, Long]
      val bad = new collection.mutable.HashMap[Int, Fail]
      val missed = new collection.mutable.HashSet[Int]
      obeys.foreach{ case (lln, law) =>
        Try{ run(lln) } match {
          case Failure(e) => bad(lln) = Fail(law, Outcome.Error(e), None, Some(e))
          case Success(x) => x match {
            case Right(n) => if (n > 0) good(lln) = n else missed += lln
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
      Tested(good.toMap, bad.toMap, missed.toSet)
    }
  }

  private class N(var count: Int = 0) { def ++(){ count += 1 } }
  implicit class ComparesTo[A, CC](me: CC)(implicit onceCC: CC => collection.TraversableOnce[A]) {
    def theSameAs[DD](you: DD)(implicit onceDD: DD => collection.TraversableOnce[A]) = {
      val meB, youB = collection.mutable.ArrayBuffer.empty[A]
      onceCC(me).foreach(meB += _)
      onceDD(you).foreach(youB += _)
      meB == youB
    }
    def correspondsTo[DD](you: DD)(implicit onceDD: DD => collection.TraversableOnce[A]) = {
      val meM, youM = collection.mutable.HashMap.empty[A, N]
      onceCC(me).foreach(a => meM.getOrElseUpdate(a, new N).++)
      onceDD(you).foreach(a => youM.getOrElseUpdate(a, new N).++)
      meM.forall{ case (a, n) => youM.get(a).exists(_.count == n.count) } &&
      youM.forall{ case (a, n) => meM contains a }
    }
    def isPartOf[DD](you: DD)(implicit onceDD: DD => collection.TraversableOnce[A]) = {
      val meM, youM = collection.mutable.HashMap.empty[A, N]
      onceCC(me).foreach(a => meM.getOrElseUpdate(a, new N).++)
      onceDD(you).foreach(a => youM.getOrElseUpdate(a, new N).++)
      meM.forall{ case (a, n) => youM.get(a).exists(_.count >= n.count) }
    }
  }
  implicit class Logic(lhs: Boolean) {
    def implies(rhs: => Boolean) = !lhs || rhs
    def impliedBy(rhs: => Boolean) = lhs || !rhs
  }
}


/////////////////////////////////////////
// Selection of element type for tests //
/////////////////////////////////////////

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

abstract class LongStrTest[CC, T <: LongStrTest[CC, T]](
  numb: Numbers, oper: Ops[(Long, String), (String, Long)], inst: Instance[(Long, String), CC], lln: Int
)(
  implicit file: sourcecode.File, line: sourcecode.Line, name: sourcecode.Name
)
extends Test[(Long, String), (String, Long), CC, T](numb, oper, inst, lln)(file, line, name) {
  type A = (Long, String)
  type B = (String, Long)
  type Inst = Instance[(Long, String), CC]
  type Oper = Ops[(Long, String), (String, Long)]

  def maxOf(a: (Long, String), aa: (Long, String)) = { val o = implicitly[Ordering[(Long, String)]]; o.max(a, aa) }
  def minOf(a: (Long, String), aa: (Long, String)) = { val o = implicitly[Ordering[(Long, String)]]; o.min(a, aa) }
  override def intFrom(kv: (Long, String)) = kv._1.toInt
}

abstract class StrLongTest[CC, T <: StrLongTest[CC, T]](
  numb: Numbers, oper: Ops[(String, Long), (Long, String)], inst: Instance[(String, Long), CC], lln: Int
)(
  implicit file: sourcecode.File, line: sourcecode.Line, name: sourcecode.Name
)
extends Test[(String, Long), (Long, String), CC, T](numb, oper, inst, lln)(file, line, name) {
  type A = (String, Long)
  type B = (Long, String)
  type Inst = Instance[(String, Long), CC]
  type Oper = Ops[(String, Long), (Long, String)]

  def maxOf(a: (String, Long), aa: (String, Long)) = { val o = implicitly[Ordering[(String, Long)]]; o.max(a, aa) }
  def minOf(a: (String, Long), aa: (String, Long)) = { val o = implicitly[Ordering[(String, Long)]]; o.min(a, aa) }
  override def intFrom(kv: (String, Long)) = kv._2.toInt
}

