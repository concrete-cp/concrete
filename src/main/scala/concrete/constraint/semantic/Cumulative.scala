package concrete
package constraint
package semantic

import java.util

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging

trait CumulativeChecker extends Constraint with LazyLogging {
  def nbTasks: Int

  def check(tuple: Array[Int]): Boolean = {

    val s = tuple.slice(0, nbTasks)
    val d = tuple.slice(nbTasks, 2 * nbTasks)
    val r = tuple.slice(2 * nbTasks, 3 * nbTasks)
    val b = tuple(3 * nbTasks)



    // logger.warn(id + ". " + Seq(s, d, r).map(_.mkString("[", ", ", "]")).toString + " " + b);

    // println((s.toSeq, d.toSeq, r.toSeq, b))

    //    val tasks = s.indices.filter(i => r(i) > 0 && d(i) > 0)
    //    val c = tasks.isEmpty || {

    val late = (s lazyZip d).map(_ + _).max

    val profile = new Array[Int](late)

    var i = nbTasks - 1
    while (i >= 0) {
      var j = d(i) - 1
      while (j >= 0) {
        profile(s(i) + j) += r(i)
        if (profile(s(i) + j) > b) {
          return false
        }
        j -= 1
      }
      i -= 1
    }
    true
  }
}

/**
  * Requires that a set of tasks given by start times s, durations d, and
  * resource requirements r, never require more than a global resource bound
  * b at any one time.
  *
  * Assumptions:
  * - forall i, d[i] >= 0 and r[i] >= 0
  */

class Cumulative(startTimes: Array[Variable], d: Array[Variable], r: Array[Variable], b: Variable) extends Constraint(startTimes ++ d ++ r :+ b)
  with BC with CumulativeChecker with FixPoint {

  private var begin: Int = _
  private var profile: Array[Int] = _

  override def toString(ps: ProblemState): String = {
    s"Cumulative(start = [${startTimes.map(ps.dom).mkString(", ")}], dur = [${d.map(ps.dom).mkString(", ")}], res = [${r.map(ps.dom).mkString(", ")}], bound = ${ps.dom(b)})"
  }

  def advise(problemState: ProblemState, pos: Int): Int = arity * arity

  def init(ps: ProblemState): Outcome = {
    val startDomains = ps.doms(startTimes)
    begin = startDomains.map(_.head).min
    val end = (startDomains lazyZip ps.doms(d)).map((s, d) => s.last + d.last).max
    profile = new Array[Int](end - begin + 1)
    ps
  }

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {
    buildProfile(ps)
      .andThen(ps =>
        fixPoint(ps, startTimes.indices,
          (ps, i) => filterB(ps, ps.dom(b).last, i) //.andThen(s => filterR(s, s.dom(b).last, i))
        )
      )
  }

  private def buildProfile(ps: ProblemState): Outcome = {
    util.Arrays.fill(profile, 0)
    var bound = ps.dom(b).head

    val maxBound = ps.dom(b).last

    var i = nbTasks - 1

    while (bound <= maxBound && i >= 0) {
      val sDom = ps.dom(startTimes(i))
      val dBound = ps.dom(d(i)).head
      val rBound = ps.dom(r(i)).head

      // partie obligatoire entre debut au plus tard et fin au plus tôt
      var j = sDom.last
      val earliestEnd = sDom.head + dBound
      while (bound <= maxBound && j < earliestEnd) {
        profile(j - begin) += rBound
        bound = math.max(bound, profile(j - begin))
        j += 1
      }
      i -= 1
    }

    ps.removeUntil(b, bound)
  }

  def nbTasks: Int = startTimes.length

  private def filterB(state: ProblemState, bound: Int, i: Int): Outcome = {
    val sDom = state.dom(startTimes(i))
    val dBound = state.dom(this.d(i)).head
    val rDom = state.dom(r(i))
    val rBound = rDom.head

    // Remove current task from profile
    for (i <- sDom.last until (sDom.head + dBound)) {
      profile(i - begin) -= rBound
    }

    //println(profile.mkString(" ") + " <= " + bound)

    // Sweep left
    findLeftPosition(profile, sDom, sDom.head, dBound, rBound, bound) match {
      case None => Contradiction(startTimes(i))
      case Some(min) =>

        // Find upper bound for resource
        var start = min
        val rMax = rDom.iterator.drop(1).find { r =>
          // println(s"Searching for position of height $r")
          val support = findLeftPosition(profile, sDom, start, dBound, r, bound)
          support.foreach(start = _)
          support.isEmpty
        }


        // Sweep right
        var max = sDom.last + dBound - 1
        var d = 0
        while (d < dBound) {
          if (profile(max - d - begin) + rBound > bound) {
            max = sDom.prev(max - d - dBound + 1) + dBound - 1
            d = 0
          } else {
            d += 1
          }
        }

        val filtered = sDom & (min, max - dBound + 1)



        // Should no longer happen even if there are "holes" in the domains
        assert(filtered.nonEmpty)
        var minBound = state.dom(b).head


        var ps = state.updateDom(startTimes(i), filtered).fold(rMax)(_.removeFrom(r(i), _))
        var j = filtered.last
        while (j < filtered.head + dBound && ps.isState) {
          profile(j - begin) += rBound
          if (profile(j - begin) > minBound) {
            minBound = profile(j - begin)
            ps = ps.removeUntil(b, profile(j - begin))
          }
          j += 1
        }
        ps
    }
  }

  //  private def filterR(state: ProblemState, bound: Int, i: Int): Outcome = {
  //    val sDom = state.dom(startTimes(i))
  //    val dBound = state.dom(this.d(i)).head
  //    val rDom = state.dom(r(i))
  //
  //
  //    //println(s"Filtering $i: ${r(i).toString(state)}")
  //
  //
  //    // Remove current task from profile
  //    for (i <- sDom.last until (sDom.head + dBound)) {
  //      profile(i - begin) -= rDom.head
  //    }
  //
  //    var start = sDom.head
  //    val rMin = rDom.iterator.drop(1).find { r =>
  //      // println(s"Searching for position of height $r")
  //      val support = findLeftPosition(profile, sDom, start, dBound, r, bound)
  //      support.foreach(start = _)
  //
  //      support.isEmpty
  //    }
  //
  //    //  println(rMin.map(b => s"Could not find position for height $b"))
  //
  //    // Reinsert current task in profile
  //    for (i <- sDom.last until (sDom.head + dBound)) {
  //      profile(i - begin) += rDom.head
  //    }
  //
  //    rMin.map(state.removeFrom(r(i), _))
  //      .getOrElse(state)
  //
  //  }

  private def findLeftPosition(profile: Array[Int], sDom: Domain, start: Int, duration: Int, r: Int, bound: Int): Option[Int] = {
    var min = start
    var d = 0
    val last = sDom.last
    while (d < duration) {
      if (profile(min + d - begin) + r > bound) {
        if (min + d < last) {
          min = sDom.next(min + d)
        } else {
          return None
        }
        d = 0
      } else {
        d += 1
      }
    }
    Some(min)
  }

  def simpleEvaluation: Int = 3


}