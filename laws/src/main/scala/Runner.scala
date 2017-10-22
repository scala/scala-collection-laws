package laws

import scala.util.control.NonFatal

class Runner[A, B, CC, T <: Test[A, B, CC, T]](
  lawLine: Int,
  exploreInst: () => Exploratory[Instance[A, CC]],
  exploreOps: () => Exploratory[Ops[A, B]],
  testGen: (Int, Instance[A, CC], Ops[A, B], Numbers) => T
) {
  def exploreNum(inst: Instance[A, CC]) =
    new Numbers.Restricted(inst.secret.xsize, inst.secret.ysize)

  def runOne(inst: Instance[A, CC], oper: Ops[A, B], num: Numbers): Option[Either[(T, Throwable), T]] = {
    val t: T = testGen(lawLine, inst, oper, num)
    try { if (t.run.exists(_ == false)) Some(Right(t)) else None }
    catch { case e if NonFatal(e) => Some(Left((t, e))) }
  }
  
  def runNums(inst: Instance[A, CC], oper: Ops[A, B]): Either[Either[(T, Throwable), T], Int] = {
    val mkNum = exploreNum(inst)
    val exNum = mkNum.explore
    var progress = true
    var n = 0
    while (progress) {
      mkNum.lookup(exNum) match {
        case Some(num) =>
          n += 1
          val before = num.count
          runOne(inst, oper, num) match {
            case Some(err) => return Left(err)
            case _ =>
          }
          val after = num.count
          val touched = after isnt before
          exNum.advance(touched)
        case None =>
          progress = false
      }
    }
    Right(n)
  }

  def runInst(inst: Instance[A, CC]): Either[Either[(T, Throwable), T], Long] = {
    val mkOps = exploreOps()
    val exOps = mkOps.explore
    var progress = true
    var n = 0L
    while (progress) {
      mkOps.lookup(exOps) match {
        case Some(oper) =>
          val before = oper.count
          runNums(inst, oper) match {
            case Left(err) => return Left(err)
            case Right(k) => n += k
          }
          val after = oper.count
          val touched = after isnt before
          exOps.advance(touched)
        case None =>
          progress = false
      }
    }
    Right(n)
  }

  def run: Either[Either[(T, Throwable), T], Long] = {
    val mkInst = exploreInst()
    val exInst = mkInst.explore
    var progress = true
    var n = 0L
    while (progress) {
      mkInst.lookup(exInst) match {
        case Some(inst) =>
          val before = inst.count
          runInst(inst) match {
            case Left(err) => return Left(err)
            case Right(k) => n += k
          }
          val after = inst.count
          val touched = after isnt before
          exInst.advance(touched)
        case None =>
          progress = false
      }
    }
    Right(n)
  }
}
