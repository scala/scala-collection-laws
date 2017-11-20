package laws

/** Marker trait for results of running a test of a law */
sealed trait Outcome {}

/** Handles results of tests including, most importantly,
  * capturing relevant information about failed tests.
  */
object Outcome {
  /** Indicates that a law passed a test. */
  final case object Success extends Outcome {}

  /** Indicates that the test values were not appropriate for this law,
    * and suggests what value to change to try to make them appropriate.
    *
    * These handle things that can only be known at runtime because not
    * _all_ possible combinations of values are out of bounds.
    */
  sealed abstract class Skip extends Outcome { def accessed: Array[Boolean] }
  object Skip {
    /** Indicates that the `Number` values were inappropriate for this law. */
    final class Num(n: Boolean, nn: Boolean, m: Boolean, mm: Boolean, r: Boolean) extends Skip { 
      val accessed = Array(n, nn, m, mm, r)
    }
    object Num {
      def unapply(num: Num): Option[Array[Boolean]] = Some(num.accessed)
    }

    /** Indicates that the functions/operators were inappropriate for this law. */
    final class Oper(f: Boolean, g: Boolean, op: Boolean, p: Boolean, pf: Boolean) extends Skip { 
      val accessed = Array(f, g, op, p, pf)
    }
    object Oper {
      def unapply(oper: Oper): Option[Array[Boolean]] = Some(oper.accessed)
    }

    /** Indicates that the particular element or collection was inappropriate for this law.
      *
      * Note that this is normally taken care of using flags, so the test isn't even generated.
      */
    final class Inst(a: Boolean, x: Boolean, y: Boolean) extends Skip { 
      val accessed = Array(a, x, y)
    }
    object Inst {
      def unapply(inst: Inst): Option[Array[Boolean]] = Some(inst.accessed)
    }

    /** Vary `Numbers`' `n` parameter to try to find an acceptable value */
    val n  = new Num(true, false, false, false, false)

    /** Vary `Numbers`' `nn` parameter to try to find an acceptable value */
    val nn = new Num(false, true, false, false, false)

    /** Vary `Numbers`' `m` parameter to try to find an acceptable value */
    val m  = new Num(false, false, true, false, false)

    /** Vary `Numbers`' `mm` parameter to try to find an acceptable value */
    val mm = new Num(false, false, false, true, false)

    /** Vary `Numbers`' `r` parameter to try to find an acceptable value */
    val r  = new Num(false, false, false, false, true)


    /** Vary `Ops`' `f` parameter to try to find an acceptable operation */
    val f  = new Oper(true, false, false, false, false)

    /** Vary `Ops`' `g` parameter to try to find an acceptable operation */
    val g  = new Oper(false, true, false, false, false)

    /** Vary `Ops`' `op` parameter to try to find an acceptable operation */
    val op = new Oper(false, false, true, false, false)

    /** Vary `Ops`' `p` parameter to try to find an acceptable operation */
    val p  = new Oper(false, false, false, true, false)

    /** Vary `Ops`' `pf` parameter to try to find an acceptable operation */
    val pf = new Oper(false, false, false, false, true)

    /** Vary `Instance`'s `a` parameter to try to find an acceptable element */
    val a  = new Inst(true, false, false)

    /** Vary `Instance`'s `x` parameter to try to find an acceptable collection */
    val x  = new Inst(false, true, false)

    /** Vary `Instance`'s `y` parameter to try to find an acceptable collection */
    val y  = new Inst(false, false, true)
  }

  /** The requested law did not run successfully because it does not even
    * exist on the specified line!
    */
  final case class Missing(lawLine: Int) extends Outcome {}

  /** The test ran, but failed. */
  final case class Failed[T](test: T) extends Outcome {}

  /** The test was attempted but it threw an exception while it was running. */
  final case class Threw[T](test: T, error: Throwable) extends Outcome {}

  /** An exception was thrown while getting ready to run a test, but the test didn't actually run.
    *
    * This should never happen unless there's a bug in the testing framework (or a very bad library bug).
    */
  final case class Error(error: Throwable) extends Outcome {}
}
