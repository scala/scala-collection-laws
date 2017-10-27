package laws

import scala.util.control.NonFatal

class Runner[A, B, CC, T <: Test[A, B, CC, T]](
  lawLine: Int,
  exploreInst: () => Exploratory[Instance[A, CC]],
  exploreOps: () => Exploratory[Ops[A, B]],
  testGen: (Int, Instance[A, CC], Ops[A, B], Numbers) => T
) {
  lazy val law = Laws.byLineNumber(lawLine)

  def exploreNum(inst: Instance[A, CC]) =
    new Numbers.Restricted(inst.values.xsize, inst.values.ysize)

  def runOne(inst: Instance[A, CC], oper: Ops[A, B], num: Numbers): Outcome = {
    val t: T = testGen(lawLine, inst, oper, num)
    try {
      val law = Laws.byLineNumber.get(lawLine).getOrElse(return Outcome.Missing(lawLine))
      law.tags.validate(t).foreach{ skip => return skip }
      if (t.run) Outcome.Success else Outcome.Failed(t)
    }
    catch { case e if NonFatal(e) => Outcome.Threw(t, e) }
  }

  def runNums(inst: Instance[A, CC], oper: Ops[A, B]): Either[Outcome, Int] = {
    val mkNum = exploreNum(inst)
    val exNum = mkNum.explore
    var progress = true
    var n = 0
    while (progress) {
      mkNum.lookup(exNum) match {
        case Some(num) =>
          num.setUnused()
          runOne(inst, oper, num) match {
            case Outcome.Success =>
              exNum.advance(num.used)
              n += 1
            case Outcome.Skip.Num(blame) =>
              exNum.advance(blame)
            case o =>
              return Left(o)
          }
        case None =>
          progress = false
      }
    }
    Right(n)
  }

  def runInst(inst: Instance[A, CC]): Either[Outcome, Long] = {
    val mkOps = exploreOps()
    val exOps = mkOps.explore
    var progress = true
    var n = 0L
    while (progress) {
      mkOps.lookup(exOps) match {
        case Some(oper) =>
          oper.setUnused()
          runNums(inst, oper) match {
            case Right(k) =>
              exOps.advance(oper.used)
              n += k
            case Left(Outcome.Skip.Oper(blame)) =>
              exOps.advance(blame)
            case Left(err) =>
              return Left(err)
          }
        case None =>
          progress = false
      }
    }
    Right(n)
  }

  def run: Either[Outcome, Long] = {
    val mkInst = exploreInst()
    val exInst = mkInst.explore
    var progress = true
    var n = 0L
    while (progress) {
      mkInst.lookup(exInst) match {
        case Some(inst) =>
          inst.setUnused()
          runInst(inst) match {
            case Right(k) => 
              exInst.advance(inst.used)
              n += k
            case Left(Outcome.Skip.Inst(blame)) =>
              exInst.advance(blame)
            case Left(err) =>
              return Left(err)
          }
        case None =>
          progress = false
      }
    }
    Right(n)
  }
}
