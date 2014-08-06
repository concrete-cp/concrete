package concrete.constraint.semantic

import concrete.Variable
import concrete.util.Interval
import concrete.constraint.Constraint
import com.typesafe.scalalogging.LazyLogging
import concrete.UNSATObject
import scala.collection.mutable.TreeSet
import concrete.Domain
import scala.collection.mutable.ArrayBuffer
import java.util.Arrays
import scala.util.Sorting
import scala.collection.mutable.ArrayBuilder

/**
 * Cumulative implements the cumulative/4 constraint using edge-finding
 * algorithm and profile information on the resource use.
 *
 * Borrowed from JaCoP
 *
 * @author Krzysztof Kuchcinski and Radoslaw Szymanek
 * @version 3.0
 */

case class IntTask(var start: Int = 0, var stop: Int = 0)

case class Task(val start: Variable, val dur: Variable, val res: Variable) {

  def areaMax = dur.dom.lastValue * res.dom.lastValue
  def areaMin = dur.dom.firstValue * res.dom.firstValue
  def Compl = {
    val sDom = start.dom
    val dDom = dur.dom
    Interval(sDom.firstValue + dDom.firstValue, sDom.lastValue + dDom.lastValue)
  }

  def Completion = {
    val sDom = start.dom
    val dDomMin = dur.dom.firstValue
    Interval(sDom.firstValue + dDomMin, sDom.lastValue + dDomMin)
  }

  def ECT = start.dom.firstValue + dur.dom.firstValue

  def EST = start.dom.firstValue

  def LaCT = start.dom.lastValue + dur.dom.lastValue

  def LCT = start.dom.lastValue + dur.dom.firstValue

  def LST = start.dom.lastValue

  def minUse(t: IntTask) = {
    val lst = LST
    val ect = ECT
    if (LST < ECT) {
      t.start = lst;
      t.stop = ect;
      true
    } else false
  }
}

class Cumulative(
  val starts: Array[Variable],
  val durations: Array[Variable],
  val resources: Array[Variable],
  val limit: Variable) extends Constraint(starts ++ durations ++ resources :+ limit) with LazyLogging {
  //  /**
  //   * It contains information about maximal profile contributed by tasks.
  //   */
  //  var maxProfile: Profile = null
  //
  //  /**
  //   * It contains information about minimal profile contributed by regions
  //   * for certain occupied by tasks.
  //   */
  //  var minProfile: Profile = null
  //
  //  var cumulativeProfiles: CumulativeProfiles = new CumulativeProfiles

  //  /**
  //   * It specifies if the edge finding algorithm should be used.
  //   */
  //  val doEdgeFinding = true;

  //  /**
  //   * It specifies if the profiles should be computed to propagate
  //   * onto limit variable.
  //   */
  //  val doProfile = true;
  def advise(pos: Int): Int = this.scopeSize
  def checkValues(tuple: Array[Int]): Boolean = {
    val s = tuple.take(starts.length)
    val d = tuple.slice(starts.length, starts.length + durations.length)
    val r = tuple.slice(starts.length + durations.length, starts.length + durations.length + resources.length)
    val b = tuple.last

    val tasks = s.indices.filter(i => r(i) > 0 && d(i) > 0)
    val times = tasks.map(s).min to tasks.map(i => s(i) + d(i)).max
    times.forall(t =>
      b >= tasks
        .map { i =>
          if (s(i) <= t && t < s(i) + d(i)) {
            r(i)
          } else {
            0
          }
        }
        .sum)
    //      
    //    let { 
    //               set of int: tasks = 
    //                  {i | i in index_set(s) where ub(r[i]) > 0 /\ ub(d[i]) > 0 },
    //               set of int: times =
    //                  min([ lb(s[i]) | i in tasks ]) ..
    //                  max([ ub(s[i]) + ub(d[i]) | i in tasks ]) 
    //                } 
    //            in
    //               forall( t in times ) (
    //                  b >= sum( i in tasks ) (
    //                     bool2int( s[i] <= t /\ t < s[i] + d[i] ) * r[i]
    //                  )
    //               )

  }
  def simpleEvaluation: Int = 3
  /**
   * It specifies if the data from profiles should be used to propagate
   * onto limit variable.
   */
  val setLimit = true;

  require(starts.length == durations.length, "Starts and durations list have different length")
  require(resources.length == durations.length, "Resources and durations list have different length")

  require(durations.forall(_.dom.firstValue >= 0), "durations must be positive")
  require(resources.forall(_.dom.firstValue >= 0), "resources must be positive")
  require(limit.dom.firstValue >= 0, "limit must be positive")

  val Ts = (starts, durations, resources).zipped.map(Task)

  private def after(l: Task, S: IndexedSeq[Task]): Boolean = {

    var startS = Int.MaxValue

    var a = 0L;
    var afterS = true;

    if (S.size > 0) {
      logger.debug(s"Checking if $l can be after $S");
      for (t <- S) {
        val tEST = t.EST
        if (tEST <= startS) {
          startS = tEST
        }
        a += t.areaMin
      }

      afterS = ((l.LCT - startS) * limit.dom.lastValue - a >= l.areaMin);

      logger.debug(s"s(S') = $startS, c(l) = ${l.LCT}, a(Sp) = $a, afterS = $afterS")
    }
    afterS;
  }

  private def before(l: Task, S: IndexedSeq[Task]): Boolean = {
    var completionS = Int.MinValue;
    var a = 0;
    var beforeS = true;

    if (S.size > 0) {
      logger.debug(s"Checking if $l can be before tasks in $S");
      for (t <- S) {
        val tLCT = t.LCT
        if (tLCT >= completionS) {
          completionS = tLCT
        }
        a += t.areaMin
      }

      beforeS = ((completionS - l.EST) * limit.dom.lastValue >= a + l.areaMin)

      logger.debug(s"s(l) = ${l.EST}, c(S') = $completionS, a(Sp) = $a, beforeS = $beforeS")
    }
    beforeS;
  }

  private def between(l: Task, S: IndexedSeq[Task]): Boolean = {
    var completionS = Int.MinValue
    var startS = Int.MaxValue
    var a = 0L
    var larea = 0L
    var betweenS = true;

    if (S.size > 0) {
      logger.debug(s"Checking if $l can be between tasks in $S");
      for (t <- S) {
        val tLCT = t.LCT
        val tEST = t.EST
        if (tLCT >= completionS) {
          completionS = tLCT
        }
        if (tEST <= startS) {
          startS = tEST
        }
        a += minOverlap(t, startS, completionS);
      }
      larea = minOverlap(l, startS, completionS);

      betweenS = ((completionS - startS) * limit.dom.lastValue >= a + larea);

      logger.debug(s"s(S') = $startS, c(S') = $completionS, a(Sp) = $a, l_area= $larea, betweenS= $betweenS")
    }
    return betweenS;
  }

  def revise(): Traversable[Int] = {

    var ch = Set[Int]()

    do {
      val doms = sizes()

      // Phase-up - from highest lct down
      edgeFindingUp();
      // Phase-down - from lowest est up
      edgeFindingDown();

      val thisch = (0 until arity).filter(i => scope(i).dom.size != doms(i))

      if (thisch.isEmpty) {
        if (!controlAssignment) {
          throw UNSATObject
        } else {
          return ch
        }
      }
      ch ++= thisch
    } while (true)

    throw new IllegalStateException
    //
    //    var propagationHasOccurred = false;
    //    do {
    //
    //      //
    //      //      if (doProfile) {
    //      //
    //      //        cumulativeProfiles.make(Ts);
    //      //
    //      //        minProfile = cumulativeProfiles.minProfile;
    //      //        if (setLimit)
    //      //          maxProfile = cumulativeProfiles.maxProfile;
    //      //        logger.debug(s"""
    //      //--------------------------------------
    //      //MinProfile for $this: $minProfile
    //      //MaxProfile for $this: $maxProfile
    //      //--------------------------------------""");
    //      //
    //      //        if (setLimit) {
    //      //          limit.dom.intersectVal(minProfile.max, maxProfile.max);
    //      //        } else if (limit.dom.lastValue < minProfile.max) {
    //      //          throw UNSATObject
    //      //        }
    //      //
    //      //        updateTasksRes(store);
    //      //
    //      //        profileCheckTasks(store);
    //      //      }
    //
    //      // Do edge finding only the last time and when
    //      // max limit is 1 (heuristic) !!!
    //      if (doEdgeFinding) {
    //
    //      }
    //
    //    } while (store.propagationHasOccurred);
    //
    //    minProfile = null;
    //    maxProfile = null;
  }

  def edgeFindingDown() {
    val estUpList = new TreeSet[Domain]()(DomainminComparator)
    val S = new ArrayBuffer[Task](Ts.length)
    val L = new ArrayBuffer[Task](Ts.length)
    var est0: Int = 0
    var t: Task = null
    var l: Task = null
    var compl: Int = 0
    var totalArea = 0
    var estS = 0
    logger.debug("------------------------------------------------\n" +
      "Edge Finding Down\n" +
      "------------------------------------------------")
    for (T <- Ts if T.dur.dom.firstValue > 0 && T.res.dom.lastValue > 0) {
      estUpList.add(T.start.dom)
    }
    for (EST <- estUpList) {
      est0 = EST.firstValue
      logger.debug("est0 = " + est0 + "\n=================")
      S.clear
      L.clear
      for (T <- Ts if T.dur.dom.firstValue > 0 && T.res.dom.firstValue > 0) {
        if (T.EST >= est0) {
          S += T
        } else if (T.LCT > est0) {
          L += T
        }
      }
      logger.debug("S = " + S)
      logger.debug("L = " + L)

      for (T <- S) notLast(T, S)
      if (S.size != 0 && !fitTasksAfter(S, est0)) {
        throw UNSATObject
      }
      while (S.size != 0 && L.size != 0) {
        val indexOfl = maxArea(L)
        l = L(indexOfl)
        val l_LCT = l.LCT
        val limitMax = limit.dom.lastValue
        var AFTER = true
        var BETWEEN = true
        var startOfS = Int.MaxValue
        var completionOfS = Int.MinValue
        var area1 = 0
        var area2 = 0L
        var larea = 0L
        logger.debug("Checking if " + l + " can be after " + S)
        for (tt <- S) {
          val tEST = tt.EST
          val tLCT = tt.LCT
          if (tEST <= startOfS) startOfS = tEST
          if (tLCT >= completionOfS) completionOfS = tLCT
          area1 += tt.areaMin
          area2 += minOverlap(tt, startOfS, completionOfS)
        }
        totalArea = area1
        estS = startOfS
        AFTER = ((l_LCT - startOfS) * limitMax - area1 >= l.areaMin)
        larea = minOverlap(l, startOfS, completionOfS)
        BETWEEN = ((completionOfS - startOfS) * limitMax >= area2 + larea)
        if (AFTER && BETWEEN) {
          L.remove(indexOfl)
          removeFromS_Lct(S)
        } else {
          if (BETWEEN && !AFTER) {
            var slack: Long = 0l
            var a = 0
            var newCompl = Int.MaxValue
            var startS: Int = 0
            var newStartl = Int.MinValue
            val maxuse = limitMax - l.res.dom.firstValue
            compl = l_LCT
            a = totalArea
            startS = estS
            slack = (l_LCT - startS) * limitMax - a - l.areaMin
            var j = 0
            val Tasks = new ArrayBuffer[Task]
            while (slack < 0 && j < S.size) {
              t = S(j)
              if (t.res.dom.firstValue <= maxuse || l_LCT <= t.LST) {
                slack += t.areaMin
              } else {
                Tasks += t
              }
              j += 1
            }
            if (slack < 0 && Tasks.size != 0) {
              val TaskArr = Tasks.toArray
              Arrays.sort(TaskArr, TaskDescLSTComparator)
              j = 0
              val limitMin = limit.dom.firstValue
              while (slack < 0 && j < TaskArr.length) {
                t = TaskArr(j).asInstanceOf[Task]
                j += 1
                newCompl = t.LST
                slack = slack - (compl - newCompl) * limitMin + t.areaMin
                compl = newCompl
              }
              newStartl = compl - l.dur.dom.firstValue
              if (newStartl < l.LST) {
                logger.trace(">>> Cumulative EF <<< 2. Narrowed " + l.start + " in " +
                  Int.MinValue +
                  ".." +
                  newStartl)
                l.start.dom.removeAfterVal(newStartl)
              }
            }
            if (before(l, S)) { L.remove(indexOfl) } else { removeFromS_Lct(S) }
          } else {
            if (!BETWEEN && AFTER) L.remove(indexOfl) else {
              logger.debug("AFTER=" + AFTER + " BETWEEN=" + BETWEEN + "!!!")
              var durationOfS = 0
              compl = 0
              for (T <- S) {
                durationOfS += T.dur.dom.firstValue * T.res.dom.firstValue
                if (T.LCT > compl) {
                  compl = T.LCT
                }
              }
              val finish = compl -
                (durationOfS + l.dur.dom.firstValue * l.res.dom.firstValue) / limit.dom.lastValue
              if (l.start.dom.lastValue > finish) {
                logger.trace(l + " must be before\n" + S + "\n>>> Cumulative EF <<< 3. Narrowed " +
                  l.start +
                  " in " +
                  Int.MinValue +
                  ".." +
                  finish)
                l.start.dom.removeAfterVal(finish)
              }
              L.remove(indexOfl)
            }
          }
        }
      }
    }
  }

  def edgeFindingUp() {
    // val lctDownList = new TreeSet[Interval]()(IntervalmaxComparator)
    val S = new ArrayBuffer[Task]
    val L = new ArrayBuffer[Task]

    logger.debug("------------------------------------------------\n" +
      "Edge Finding Up\n" +
      "------------------------------------------------")
    val lctDownList = Ts.view
      .filter(T => T.dur.dom.firstValue > 0 && T.res.dom.firstValue > 0)
      .map(_.Completion)
      .sorted(IntervalmaxComparator)
    //for (T <- Ts if T.dur.dom.firstValue > 0 && T.res.dom.firstValue > 0) { lctDownList.add(T.Completion) }
    for (LCT <- lctDownList) {
      val lct0 = LCT.ub
      logger.debug("lct0 = " + lct0 + "\n=================")

      S.clear()
      L.clear()
      for (T <- Ts) {
        if (T.dur.dom.firstValue > 0 && T.res.dom.firstValue > 0) {
          if (T.LCT <= lct0) S += T
          else if (T.EST < lct0) L += T
        }
      }
      logger.debug("S = " + S)
      logger.debug("L = " + L)

      for (T <- S) notFirst(T, S)
      if (S.size != 0 && !fitTasksBefore(S, lct0)) throw UNSATObject
      while (S.size != 0 && L.size != 0) {
        val indexOfl = maxArea(L)
        val l = L(indexOfl)
        val l_EST = l.EST
        val limitMax = limit.dom.lastValue
        // var BEFORE = true
        // var BETWEEN = true
        var completionOfS = Int.MinValue
        var startOfS = Int.MaxValue
        var area1 = 0L
        var area2 = 0L
        //var larea = 0L
        logger.debug("Checking if " + l + " can be before or between tasks in " +
          S)
        for (tt <- S) {
          val tLCT = tt.LCT
          val tEST = tt.EST
          if (tLCT >= completionOfS) completionOfS = tLCT
          area1 += tt.areaMin
          if (tEST <= startOfS) startOfS = tEST
          area2 += minOverlap(tt, startOfS, completionOfS)
        }

        val lctS = completionOfS
        val BEFORE = ((completionOfS - l_EST) * limitMax >= area1 + l.areaMin)
        val larea = minOverlap(l, startOfS, completionOfS)
        val BETWEEN = ((completionOfS - startOfS) * limitMax >= area2 + larea)
        logger.debug("BEFORE=" + BEFORE + " BETWEEN=" + BETWEEN + " completionOfS=" +
          completionOfS +
          " startOfS=" +
          startOfS +
          " area2=" +
          area2 +
          "  larea=" +
          larea +
          "\nS = " +
          S +
          "\nl = " +
          l)
        if (BEFORE && BETWEEN) {
          L.remove(indexOfl)
          removeFromS_Est(S)
        } else if (BETWEEN && !BEFORE) {
          var newStartl = Int.MinValue
          val maxuse = limitMax - l.res.dom.firstValue
          var startl = l_EST

          val completionS = lctS
          var slack = (completionS - l_EST) * limitMax - area1 - l.areaMin
          var j = 0
          val Tasks = new ArrayBuffer[Task]
          while (slack < 0 && j < S.size) {
            val t = S(j)
            if (t.res.dom.firstValue <= maxuse || l_EST >= t.ECT) {
              slack += t.areaMin
            } else {
              Tasks += t
            }
            j += 1
          }
          if (slack < 0 && Tasks.size != 0) {
            val TaskArr = Tasks.toArray
            Arrays.sort(TaskArr, TaskAscECTComparator)
            j = 0
            val limitMin = limit.dom.firstValue
            while (slack < 0 && j < TaskArr.length) {
              val t = TaskArr(j)
              j += 1
              newStartl = t.ECT
              slack = slack - (newStartl - startl) * limitMin + t.areaMin
              startl = newStartl
            }
          }
          if (newStartl > l_EST) {
            logger.trace(">>> Cumulative EF <<< 0. Narrowed " + l.start + " in " +
              startl +
              ".." +
              Int.MaxValue)
            l.start.dom.removeUntilVal(newStartl)
          }
          if (after(l, S)) L.remove(indexOfl) else removeFromS_Est(S)
        } else if (!BETWEEN && BEFORE) {
          L.remove(indexOfl)
        } else {
          logger.debug("BEFORE=" + BEFORE + " BETWEEN=" + BETWEEN + "!!!")
          var durationOfS = 0
          for (T <- S) durationOfS += T.dur.dom.firstValue * T.res.dom.firstValue
          val start = startOfS + durationOfS / limit.dom.lastValue
          if (start > l.start.dom.firstValue) {
            logger.trace(l + " must be after\n" + S + "\n>>> Cumulative EF <<< 1. Narrowed " +
              l.start +
              " in " +
              start +
              ".." +
              Int.MaxValue)
            l.start.dom.removeUntilVal(start)
          }
          L.remove(indexOfl)
        }

      }

    }
  }

  def est(S: ArrayBuffer[Task]): Int = S.iterator.map(_.EST).min
  //  {
  //    var estS = Int.MaxValue
  //    for (t <- S) {
  //      val tEST = t.EST
  //      if (tEST < estS) estS = tEST
  //    }
  //    estS
  //  }

  def fitTasksAfter(S: ArrayBuffer[Task], est0: Int): Boolean = {
    var durOfS = 0
    var lctOfS = Int.MinValue
    var maxCompl = Int.MaxValue
    var minDur = Int.MaxValue
    var minRes = Int.MaxValue
    var FitAfter: Boolean = false
    for (t <- S) {
      maxCompl = t.LCT
      val Dur = t.dur.dom.firstValue
      val Res = t.res.dom.firstValue
      if (lctOfS < maxCompl) lctOfS = maxCompl
      if (minDur > Dur) minDur = Dur
      if (minRes > Res) minRes = Res
      durOfS += Dur * Res
    }
    val limitMax = limit.dom.lastValue
    val availableArea = (lctOfS - est0) * limitMax
    logger.debug("Fit tasks of " + S + " after " + est0 + " = " + (availableArea >= durOfS))
    FitAfter = availableArea >= durOfS
    if (FitAfter) FitAfter = ((lctOfS - est0) / minDur) * (limitMax / minRes) >= S.size
    FitAfter
  }

  def fitTasksBefore(S: ArrayBuffer[Task], lct0: Int): Boolean = {
    var durOfS = 0
    var estOfS = Int.MaxValue
    var minStart = 0
    var minDur = Int.MaxValue
    var minRes = Int.MaxValue
    var FitBefore: Boolean = false
    for (t <- S) {
      minStart = t.EST
      val Dur = t.dur.dom.firstValue
      val Res = t.res.dom.firstValue
      if (estOfS > minStart) estOfS = minStart
      if (minDur > Dur) minDur = Dur
      if (minRes > Res) minRes = Res
      durOfS += Dur * Res
    }
    val limitMax = limit.dom.lastValue
    val availableArea = (lct0 - estOfS) * limitMax
    logger.debug("Fit tasks of " + S + " before " + lct0 + " = " + " Available are: " +
      availableArea +
      " Area: " +
      durOfS)
    FitBefore = availableArea >= durOfS
    if (FitBefore) FitBefore = ((lct0 - estOfS) / minDur) * (limitMax / minRes) >= S.size
    FitBefore
  }

  def intervalOverlap(min1: Int,
    max1: Int,
    min2: Int,
    max2: Int): Boolean = !(min1 >= max2 || max1 <= min2)

  def lct(S: ArrayBuffer[Task]): Int = {
    var lctS = Int.MinValue
    for (t <- S) {
      val tLCT = t.LCT
      if (tLCT > lctS) lctS = tLCT
    }
    lctS
  }

  def maxArea(Ts: ArrayBuffer[Task]): Int = {
    var area = 0
    var newArea = 0
    var index = 0
    var i = 0
    for (t <- Ts) {
      newArea = t.areaMin
      if (area < newArea) {
        area = newArea
        index = i
      }
      i += 1
    }
    index
  }

  def minOverlap(t: Task, est: Int, lct: Int): Long = {
    var tDur_min = 0
    val tdur = t.dur.dom.firstValue
    val tect = t.ECT
    val tlst = t.LST
    val temp1 = tect - est
    val temp2 = lct - tlst
    tDur_min = if (est <= tlst) if (tect >= lct) if (temp2 < tdur) temp2 else tdur else tdur else if (tect > est) if (tect <= lct) if (temp1 < tdur) temp1 else tdur else if (lct - est < tdur) lct - est else tdur else 0
    tDur_min * t.res.dom.firstValue
  }

  def notFirst(s: Task, S: IndexedSeq[Task]) {
    val sEST = s.EST
    var completionS = Int.MinValue
    var newStartl = Int.MinValue
    var startl = sEST
    var a = 0
    var slack: Long = 0l
    var maxuse = limit.dom.lastValue - s.res.dom.firstValue
    var notBeforeS: Boolean = false
    if (S.nonEmpty) {
      logger.debug("Not first " + s + " in " + S)
      for (t <- S) {
        if (t ne s) {
          val tLCT = t.LCT
          if (tLCT >= completionS) completionS = tLCT
          a += t.areaMin
        }
      }
      slack = (completionS - sEST) * limit.dom.lastValue - a - s.areaMin
      notBeforeS = (slack < 0)
      logger.debug("s(l)= " + sEST + ",  c(S')= " + completionS + ",  a(S)= " +
        a +
        ",  notBeforeS= " +
        notBeforeS)
      var j = 0
      val Tasks = new ArrayBuffer[Task]()
      while (slack < 0 && j < S.size) {
        val t = S(j)
        if (t ne s) {
          if (t.res.dom.firstValue <= maxuse || sEST >= t.ECT) {
            slack += t.areaMin
          } else {
            Tasks += t
          }
        }
        j += 1
      }
      if (slack < 0 && Tasks.nonEmpty) {
        val TaskArr = Tasks.toArray
        Arrays.sort(TaskArr, TaskAscECTComparator)
        j = 0
        val limitMin = limit.dom.firstValue
        while (slack < 0 && j < TaskArr.length) {
          val t = TaskArr(j)
          j += 1
          newStartl = t.ECT
          slack = slack - (newStartl - startl) * limitMin + t.areaMin
          startl = newStartl
        }
        if (newStartl > sEST) {
          logger.trace(">>> Cumulative EF <<< 4. Narrowed " + s.start + " in " +
            startl +
            ".." +
            Int.MaxValue)
          s.start.dom.removeUntilVal(newStartl)
        }
      }

    }
  }

  def notLast(s: Task, S: ArrayBuffer[Task]) {
    val sLCT = s.LCT
    var compl = sLCT
    var startS = Int.MaxValue
    var newCompl = Int.MaxValue
    var newStartl = Int.MinValue
    var a = 0
    var slack: Long = 0l
    var maxuse = limit.dom.lastValue - s.res.dom.firstValue
    var notLastInS: Boolean = false
    if (S.size > 0) {
      logger.debug("Not last " + s + " in " + S)
      for (t <- S if t != s) {
        val tEST = t.EST
        if (tEST <= startS) startS = tEST
        a += t.areaMin
      }
      slack = (sLCT - startS) * limit.dom.lastValue - a - s.areaMin
      notLastInS = (slack < 0)
      logger.debug("s(S')= " + startS + ",  c(l)= " + sLCT + ",  a(S)= " +
        a +
        ",  notLastInS= " +
        notLastInS)
      var j = 0
      val Tasks = new ArrayBuffer[Task]
      while (slack < 0 && j < S.size) {
        val t = S(j)
        if (t != s) {
          if (t.res.dom.firstValue <= maxuse || sLCT <= t.LST) {
            slack += t.areaMin
          } else {
            Tasks += t
          }
        }
        j += 1
      }
      if (slack < 0 && Tasks.size != 0) {
        val TaskArr = Tasks.toArray
        Arrays.sort(TaskArr, TaskDescLSTComparator)
        j = 0
        val limitMin = limit.dom.firstValue
        while (slack < 0 && j < TaskArr.length) {
          val t = TaskArr(j).asInstanceOf[Task]
          j += 1
          newCompl = t.LST
          slack = slack - (compl - newCompl) * limitMin + t.areaMin
          compl = newCompl
        }
        newStartl = compl - s.dur.dom.firstValue
        if (newStartl < s.start.dom.lastValue) {
          logger.trace(">>> Cumulative EF <<< 5. Narrowed " + s.start + " in " +
            Int.MinValue +
            ".." +
            newStartl)
          s.start.dom.removeAfterVal(newStartl)
        }
      }
    }
  }
  //
  //  def profileCheckInterval(
  //    Start: Variable,
  //    Duration: Variable,
  //    i: Interval,
  //    Resources: Variable,
  //    mustUseMin: Int,
  //    mustUseMax: Int) {
  //    for (p <- minProfile) {
  //      logger.debug("Comparing " + i + " with profile item " + p)
  //      if (intervalOverlap(i.min, i.max + Duration.dom.firstValue, p.min, p.max)) {
  //        logger.debug("Overlapping")
  //        if (limit.dom.lastValue - p.value < Resources.dom.firstValue) {
  //          if (mustUseMin != -1) {
  //            val use = new ProfileItem(mustUseMin, mustUseMax, Resources.dom.firstValue)
  //            val left = new ProfileItem
  //            val right = new ProfileItem
  //            p.subtract(use, left, right)
  //            if (left.min != -1) {
  //              val UpdateMin = left.min - Duration.dom.firstValue + 1
  //              val UpdateMax = left.max - 1
  //              if (!(UpdateMin > Start.dom.lastValue || UpdateMax < Start.dom.firstValue)) {
  //                if (debugNarr) System.out.print(">>> Cumulative Profile 7a. Narrowed " + Start + " \\ " +
  //                  new IntervalDomain(UpdateMin, UpdateMax))
  //                Start.domain.inComplement(store.level, Start, UpdateMin, UpdateMax)
  //                logger.trace(" => " + Start)
  //              }
  //            }
  //            if (right.min != -1) {
  //              val UpdateMin = right.min - Duration.dom.firstValue + 1
  //              val UpdateMax = right.max - 1
  //              if (!(UpdateMin > Start.dom.lastValue || UpdateMax < Start.dom.firstValue)) {
  //                if (debugNarr) System.out.print(">>> Cumulative Profile 7b. Narrowed " + Start + " \\ " +
  //                  new IntervalDomain(UpdateMin, UpdateMax))
  //                Start.domain.inComplement(store.level, Start, UpdateMin, UpdateMax)
  //                logger.trace(" => " + Start)
  //              }
  //            }
  //            if (Start.dom.lastValue < right.min && Start.dom.noIntervals == 1) {
  //              val rs = right.min - Start.dom.firstValue
  //              if (rs < Duration.dom.lastValue) {
  //                logger.trace(">>> Cumulative Profile 9. Narrow " + Duration + " in 0.." +
  //                  rs)
  //                Duration.domain.inMax(store.level, Duration, rs)
  //              }
  //            }
  //          } else {
  //            val UpdateMin = p.min - Duration.dom.firstValue + 1
  //            val UpdateMax = p.max - 1
  //            if (!(UpdateMin > Start.dom.lastValue || UpdateMax < Start.dom.firstValue)) {
  //              if (debugNarr) System.out.print(">>> Cumulative Profile 6. Narrowed " + Start + " \\ " +
  //                new IntervalDomain(UpdateMin, UpdateMax))
  //              Start.domain.inComplement(store.level, Start, UpdateMin, UpdateMax)
  //              logger.trace(" => " + Start)
  //            }
  //          }
  //        } else {
  //          if (mustUseMin != -1 && !(mustUseMax <= p.dom.firstValue || mustUseMin >= p.dom.lastValue)) {
  //            var offset = 0
  //            if (intervalOverlap(p.dom.firstValue, p.dom.lastValue, mustUseMin, mustUseMax)) offset = Resources.dom.firstValue
  //            logger.trace(">>> Cumulative Profile 8. Narrowed " + Resources + " in 0.." +
  //              (limit.dom.lastValue - p.value + offset).toInt)
  //            Resources.domain.in(store.level, Resources, 0, limit.dom.lastValue - p.value + offset)
  //          }
  //        }
  //      } else {
  //        if (Start.dom.lastValue < p.min && Start.dom.noIntervals == 1) {
  //          val ps = p.min - Start.dom.firstValue
  //          if (ps < Duration.dom.lastValue && limit.dom.lastValue - p.value < Resources.dom.firstValue) {
  //            logger.trace(">>> Cumulative Profile 10. Narrowed " + Duration + " in 0.." +
  //              ps)
  //            Duration.domain.inMax(store.level, Duration, ps)
  //          }
  //        }
  //      }
  //    }
  //  }
  //
  //  def profileCheckTasks() {
  //    val minUse = new IntTask
  //    for (t <- Ts) {
  //      val resUse = t.res
  //      val dur = t.dur
  //      if (dur.dom.lastValue > 0 && resUse.dom.lastValue > 0) {
  //        var A = -1
  //        var B = -1
  //        if (t.minUse(minUse)) {
  //          A = minUse.start
  //          B = minUse.stop
  //        }
  //        logger.debug("Start time = " + t.start + ", resource use = " + resUse +
  //          ", minimal use = {" +
  //          A +
  //          ".." +
  //          B +
  //          "}")
  //        val tStartDom = t.start.dom
  //        for (m <- 0 until tStartDom.noIntervals)
  //          profileCheckInterval(store, t.start, dur, tStartDom.getInterval(m),
  //            resUse, A, B)
  //      }
  //    }
  //  }

  def removeFromS_Est(S: ArrayBuffer[Task]) {
    var estS: Int = 0
    val TasksToRemove = new ArrayBuffer[Task](S.size)
    estS = est(S)
    for (t <- S if estS == t.EST) TasksToRemove += t
    S --= TasksToRemove
  }

  def removeFromS_Lct(S: ArrayBuffer[Task]) {
    var lctS: Int = 0
    val TasksToRemove = new ArrayBuffer[Task](S.size)
    lctS = lct(S)
    for (t <- S if lctS == t.LCT) {
      TasksToRemove += t
    }
    S --= TasksToRemove
  }

  //  override def satisfied: Boolean = {
  //    var T: Task = null
  //    var sat = true
  //    if (minProfile != null && maxProfile != null) {
  //      if ((minProfile.dom.lastValue == maxProfile.dom.lastValue) && limit.singleton &&
  //        minProfile.dom.lastValue == limit.dom.firstValue) true else false
  //    } else {
  //      if (limit.singleton) {
  //        var i = 0
  //        while (sat && i < Ts.length) {
  //          T = Ts(i)
  //          i += 1
  //          sat = sat && T.start.singleton && T.dur.singleton && T.res.singleton
  //        }
  //        sat
  //      } else false
  //    }
  //  }

  override def toString: String = {
    s"Cumulative(${Ts.mkString("[", ", ", "]")}, limit = $limit)"
  }

  def updateTasksRes() {
    val limitMax = limit.dom.lastValue
    for (T <- Ts) T.res.dom.removeAfterVal(limitMax)
  }

}

object DomainmaxComparator extends Ordering[Domain] {

  def compare(o1: Domain, o2: Domain): Int =
    Ordering.Int.compare(o1.lastValue, o2.lastValue)
}

object IntervalmaxComparator extends Ordering[Interval] {

  def compare(o1: Interval, o2: Interval): Int =
    Ordering.Int.compare(o1.ub, o2.ub)
}

object DomainminComparator extends Ordering[Domain] {

  def compare(o1: Domain, o2: Domain): Int =
    Ordering.Int.compare(o2.firstValue, o2.lastValue)
}

object TaskAscECTComparator extends Ordering[Task] {

  def compare(o1: Task, o2: Task): Int =
    Ordering.Int.compare(o2.Compl.lb, o1.Compl.lb)
}

object TaskDescLSTComparator extends Ordering[Task] {

  def compare(o1: Task, o2: Task): Int =
    Ordering.Int.compare(o2.start.dom.lastValue, o1.start.dom.lastValue)
}
