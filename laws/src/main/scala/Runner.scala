package laws

class Runner[A, B, CC, T <: Test[A, B, CC, T]](
  lawLine: Int,
  gen: Generator[A, B, CC],
  testGen: (Int, Instance[A, CC], Ops[A, B], Numbers) => T
) {
  def runOne(inst: Instance[A, CC], oper: Ops[A, B], num: Numbers): Option[T] = {
    val t: T = testGen(lawLine, inst, oper, num)
    if (t.run.exists(_ == false)) Some(t) else None
  }
  def runNums(inst: Instance[A, CC], oper: Ops[A, B]): Option[T] = {
    val mkNum = gen.numberExplorer(inst)
    val exNum = mkNum.explore
    var progress = true
    while (progress) {
      mkNum.lookup(exNum) match {
        case Some(num) =>
          val before = num.count
          val result = runOne(inst, oper, num)
          val after = num.count
          val touched = after isnt before
          exNum.advance(touched)
        case None =>
          progress = false
      }
    }
    None
  }
  def runInst(inst: Instance[A, CC]): Option[T] = {
    val mkOps = gen.opsExplorer()
    val exOps = mkOps.explore
    var progress = true
    while (progress) {
      mkOps.lookup(exOps) match {
        case Some(oper) =>
          val before = oper.count
          val result = runNums(inst, oper)
          if (result.isDefined) return result
          val after = oper.count
          val touched = after isnt before
          exOps.advance(touched)
        case None =>
          progress = false
      }
    }
    None
  }
  def run: Option[T] = {
    val mkInst = gen.instanceExplorer()
    val exInst = mkInst.explore
    var progress = true
    while (progress) {
      mkInst.lookup(exInst) match {
        case Some(inst) =>
          val before = inst.count
          val result = runInst(inst)
          if (result.isDefined) return result
          val after = inst.count
          val touched = after isnt before
          exInst.advance(touched)
        case None =>
          progress = false
      }
    }
    None
  }
}
