package laws

/** Reports on the results of tests. */
object Report {
  private implicit class ClipStringWithDots(s: String) {
    def tldr(n: Int) = if (s.length <= n) s else s.take(n-3) + "..."
  }

  /** Counts how many times each law is used, given a map with all test results.
    *
    * Returns, for each line number of a valid law, the number of collections
    * for which the law passed and failed, respectively.
    */
  def countLawUsage(ran: Map[String, Test.Tested]): Map[Int, (Int, Int)] = {
    val score = laws.Laws.all.map(law => law.lineNumber -> (N(0), N(0))).toMap
    ran.foreach{ case (_, t) =>
      t.succeeded.keys.foreach(k => score(k)._1.++)
      t.failed.keys.foreach(k => score(k)._2.++)
    }
    score.map{ case (k, (ns, nf)) => (k, (ns.count, nf.count)) }
  }

  /** Counts how many times each method was used on each collection.
    *
    * Returns a map from test names to counts.  Each count is a map from method names to
    * a pair: number of laws using that method that succeeed, number of laws using that
    * method that failed.
    */
  def countMethodUsage(ran: Map[String, Test.Tested]): Map[String, Map[String, (Int, Int)]] =
    ran.map{ case (coll, result) => 
      coll -> result.findMethodUsage.map{ case (k, (ls, lf)) => (k, (ls.size, lf.size)) }
    }

  /** Produces a report on which laws weren't used in any tests. */
  def reportUnusedLaws(ran: Map[String, Test.Tested]): Vector[String] = {
    val counts = countLawUsage(ran)
    val unused = counts.filter{ case (_, (ns, nf)) => ns == 0 && nf == 0 }
    val onlyFailed = counts.filter{ case (_, (ns, nf)) => ns == 0 && nf > 0 }
    if (unused.size == 0 && onlyFailed.size == 0)
      Vector(f"All laws successfully used (${counts.map(_._2._1).sum} times)")
    else {
      (
        if (unused.size == 0) Vector.empty[String]
        else (
          f"${unused.size} law${if (unused.size==1) "" else "s"} never used" +:
          unused.toVector.sortBy(_._1).map{ case (k, _) =>
            f"  #$k%-6d${Laws.byLineNumber(k).code.lines.mkString("\u21B5 ").tldr(70)}"
          }
        )
      ) ++
      (
        if (onlyFailed.size == 0) Vector.empty[String]
        else (
          f"${onlyFailed.size} laws never succeeded on line${if (onlyFailed.size==1) "" else "s"}" +:
          onlyFailed.toVector.sortBy(_._1).map{ case (k, (_, nf)) =>
            f"  #$k%-4d failed $nf time${if (nf == 1) "" else "s" }".padTo(28, ' ') + Laws.byLineNumber(k).code.lines.mkString("\u21B5 ").tldr(50)
          }
        )
      )
    }
  }

  /** Produces a report on which methods were never tested (for each collection) */
  def reportUnusedMethods(ran: Map[String, Test.Tested]): Vector[String] = ran.
    groupBy(_._1.split('_').drop(1).take(2).mkString("_")).
    toVector.sortBy(_._1).
    flatMap{ case (title, tests) => 
      val missing =
        tests.
          map(_._2.findMethodUsage.map{ case (k, (ls, lf)) => (k, ls.size + lf.size) }).
          reduceLeft{ (l, r) =>
            val keys = l.keys ++ r.keys
            keys.toArray.
              map{ k => k -> (l.getOrElse(k, 0) + r.getOrElse(k, 0)) }.
              toMap
          }.
          collect{ case (k, n) if n == 0 => k }.
          toVector.sorted
      if (missing.isEmpty) Vector.empty[String]
      else {
        val longest = missing.map(_.length).foldLeft(1)(_ max _)
        f"Untested methods in ${title}:" +:
        missing.grouped(77/(longest+1) max 1).toVector.map{ ms =>
          "  " + ms.map(_.padTo(longest+1, ' ')).mkString
        }
      }
    }

  /** Produces a report on what laws failed.
    *
    * The result is empty if all laws succeeded.
    */
  def reportFailedLaws(ran: Map[String, Test.Tested]): Vector[String] = {
    val broken = collection.mutable.LongMap.empty[Mu[List[(String, Test.Fail)]]]
    ran.foreach{ case (name, tested) =>
      tested.failed.foreach{ case (lln, failure) =>
        broken.getOrElseUpdate(lln, Mu(List.empty[(String, Test.Fail)])).mutf((name, failure) :: _)
      }
    }
    if (broken.isEmpty) Vector.empty
    else {
      broken.toVector.sortBy(_._1).flatMap{ case (lln, errs) =>
        Vector(
          f"*****************************************",
          f"********** Failures in line $lln",
          f"*****************************************",
        ) ++
        errs.value.
          map{ case (name, failure) => f"****** $name ******\n$failure\n" }.
          mkString("\n").lines.toVector ++
        Vector("", "")
      } ++
      Vector(
        f"************************************",
        f"************************************",
        f"************** ${broken.map(_._2.value.size).sum} errors",
        f"************************************",
        f"************************************"
      )
    }
  }

  /** If we ran the tests with jUnit, the results are in a `ConcurrentHashMap`.
    * Print out the results, throwing an error afterwards if any laws failed.
    */
  def junitReport(ran: java.util.concurrent.ConcurrentHashMap[String, Test.Tested]): Unit = {
    import scala.collection.JavaConverters._
    val m = ran.asScala.toMap
    reportUnusedMethods(m).foreach(println)
    reportUnusedLaws(m).foreach(println)
    val fails = reportFailedLaws(m)
    fails.foreach(println)
    assert(fails.isEmpty)
  }
}
