package laws

sealed trait Outcome {}

object Outcome {
  final case object Success extends Outcome {}

  sealed abstract class Skip extends Outcome { def accessed: Array[Boolean] }
  object Skip {
    final class Num(n: Boolean, nn: Boolean, m: Boolean, mm: Boolean, r: Boolean) extends Skip { 
      val accessed = Array(n, nn, m, mm, r)
    }
    object Num {
      def unapply(num: Num): Option[Array[Boolean]] = Some(num.accessed)
    }

    final class Oper(f: Boolean, g: Boolean, op: Boolean, p: Boolean, pf: Boolean) extends Skip { 
      val accessed = Array(f, g, op, p, pf)
    }
    object Oper {
      def unapply(oper: Oper): Option[Array[Boolean]] = Some(oper.accessed)
    }

    final class Inst(a: Boolean, x: Boolean, y: Boolean) extends Skip { 
      val accessed = Array(a, x, y)
    }
    object Inst {
      def unapply(inst: Inst): Option[Array[Boolean]] = Some(inst.accessed)
    }

    val n  = new Num(true, false, false, false, false)
    val nn = new Num(false, true, false, false, false)
    val m  = new Num(false, false, true, false, false)
    val mm = new Num(false, false, false, true, false)
    val r  = new Num(false, false, false, false, true)

    val f  = new Oper(true, false, false, false, false)
    val g  = new Oper(false, true, false, false, false)
    val op = new Oper(false, false, true, false, false)
    val p  = new Oper(false, false, false, true, false)
    val pf = new Oper(false, false, false, false, true)

    val a  = new Inst(true, false, false)
    val x  = new Inst(false, true, false)
    val y  = new Inst(false, false, true)
  }

  final case class Missing(lawLine: Int) extends Outcome {}

  final case class Failed[T](test: T) extends Outcome {}

  final case class Threw[T](test: T, error: Throwable) extends Outcome {}
}
