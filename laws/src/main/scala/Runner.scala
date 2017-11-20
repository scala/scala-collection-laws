package laws

import scala.util.control.NonFatal

/** Runs a particular law across all relevant different parameter sets.
  *
  * In particular, the parameters are instrumented; if a parameter is
  * never used, alternate values of that parameter are not tested.
  *
  * The search strategy is a three-tiered hierarchy.  The innermost
  * traversal is across the numeric values `n`, `nn`, `m`, `mm`, and
  * `r`.  If any of these are used, alternate values are selected and
  * the test is run again.  The central traversal is across the
  * operations `f`, `g`, `op`, `p`, and `pf`.  If any of these are
  * used for any combination of the numeric values tried, alternates
  * are selected and all (relevant) numeric values are tried again.
  * The outermost traversal is across elements and collections.  If
  * `a`, `x`, `xsize`, `y`, or `ysize` are used for any of the
  * tested combinations of numeric values and operations, alternates
  * are chosen and the tests are run again.
  *
  * Generation of alternates continues until all meaningful alternatives
  * have been exhausted.
  */
class Runner[A, B, CC, T <: Test[A, B, CC, T]](
  lawLine: Int,
  exploreInst: () => Exploratory[Instance[A, CC]],
  exploreOps: () => Exploratory[Ops[A, B]],
  testGen: (Int, Instance[A, CC], Ops[A, B], Numbers) => T
) {
  /** The law being tested */
  lazy val law = Laws.byLineNumber(lawLine)

  /** Picks appropriate numeric values to traverse depending on the collections chosen. */
  def exploreNum(inst: Instance[A, CC]) =
    new Numbers.Restricted(inst.values.xsize, inst.values.ysize)

  /** Runs one test of a law with a particular set of values. */
  def runOne(inst: Instance[A, CC], oper: Ops[A, B], num: Numbers): Outcome = {
    val t: T = testGen(lawLine, inst, oper, num)
    try {
      val law = Laws.byLineNumber.get(lawLine).getOrElse(return Outcome.Missing(lawLine))
      law.tags.validate(t).foreach{ skip => return skip }
      if (t.run) Outcome.Success else Outcome.Failed(t)
    }
    catch { case e if NonFatal(e) => Outcome.Threw(t, e) }
  }

  /** Runs tests of the appropriate law generating all relevant diversity in numeric values.
    *
    * On success, returns the number of tests run.  On failure, returns the failure `Outcome`.
    */
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
              // We skipped this value, but the test didnt' fail; we just need to pick something else
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

  /** Runs tests of the appropriate law generating all relevant diversity in
    * operations and numeric values.

    * On success of all tests, returns the number of tests run.  On failure, returns
    * the failure `Outcome`.
    */
  def runOps(inst: Instance[A, CC]): Either[Outcome, Long] = {
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
              // We skipped this operation but the test didn't fail; we just need to pick something else
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

  /** Runs a test of the appropriate law generating all relevant diversity in
    * collection contents, operations, and numeric values.

    * On success of all tests, returns the number of tests run.  On failure, returns
    * the failure `Outcome`.
    */
  def run: Either[Outcome, Long] = {
    val mkInst = exploreInst()
    val exInst = mkInst.explore
    var progress = true
    var n = 0L
    while (progress) {
      mkInst.lookup(exInst) match {
        case Some(inst) =>
          inst.setUnused()
          runOps(inst) match {
            case Right(k) => 
              exInst.advance(inst.used)
              n += k
            case Left(Outcome.Skip.Inst(blame)) =>
              // We skipped this particular collection/element combination but the test didn't fail; we just need to pick something else
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

/** Runs all tests on all collections.  (Code generator creates a class than inherits from this one.) */
trait AllRunner {
  /** Array containing all tests in function form. */
  def runners: Array[() => (String, () => Test.Tested)]

  /** Runs all the tests, returning maps of test names to test results. */
  def runAll(quiet: Boolean = false ): Map[String, Test.Tested] = 
    runners.map{ f => 
      val (s, t) = f()
      if (!quiet) println(f"Running $s")
        (s, t())
    }.toMap

  /** Runs all the tests in parallel using futures, with the specified parallelism.
    *
    * Returns a map from test names to test results.
    */
  def runPar(atOnce: Int = 4, quiet: Boolean = true): Map[String, Test.Tested] = {
    import scala.util._
    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global   // Maybe make a fixed-size thread pool?
    val toRun = runners.map(_())
    val ans = collection.mutable.AnyRefMap.empty[String, Test.Tested]
    val slots = new Array[(String, Future[(String, Test.Tested)])](1 max (atOnce min toRun.length))
    var i = 0;
    var it = toRun.iterator
    while (i < slots.length && it.hasNext) {
      val (name, test) = it.next
      slots(i) = name -> Future{ (name, test()) }
      if (!quiet) println(name + " started...")
      i += 1
    }
    while (i > 0) {
      Await.ready(
        Future.firstCompletedOf((if (i < slots.length) slots take i else slots).map(_._2)),
        Duration.create(5, MINUTES)
      )
      var j = 0
      var found = false
      while (j < i) slots(j)._2.value match {
          case None => j += 1
          case Some(result) =>
            result match {
              case Success((name, tt)) => 
                ans(name) = tt
                if (!quiet) println("..." + name + " complete!")
              case Failure(e) => throw e
            }
            found = true
            if (it.hasNext) {
              val (name, test) = it.next
              slots(j) = name -> Future{ (name, test()) }
              if (!quiet) println(name + " started...")
              j += 1
            }
            else {
              if (i-1 > j) slots(j) = slots(i-1)
              i -= 1
            }
        }
      if (!found) throw new Exception("Await timed out while running: " + slots.take(i).map(_._1))
    }
    ans.toMap
  }
}

