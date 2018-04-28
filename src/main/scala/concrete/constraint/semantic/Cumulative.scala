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

    val tasks = s.indices.filter(i => r(i) > 0 && d(i) > 0)
    tasks.isEmpty || {
      val early = tasks.minBy(s)
      val late = tasks.maxBy(i => s(i) + d(i))

      (early to late).forall { t =>
        b >= tasks.map(i => if (s(i) <= t && t < s(i) + d(i)) r(i) else 0).sum
      }
    }
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

class Cumulative(s: Array[Variable], d: Array[Variable], r: Array[Variable], b: Variable) extends Constraint(s ++ d ++ r :+ b)
    with BC with CumulativeChecker with FixPoint {

  def nbTasks: Int = s.length

  private var begin: Int = _
  private var profile: Array[Int] = _

  override def toString(ps: ProblemState): String = {
    s"Cumulative(start = [${s.map(ps.dom).mkString(", ")}], dur = [${d.map(ps.dom).mkString(", ")}], res = [${r.map(ps.dom).mkString(", ")}], bound = ${ps.dom(b)})"
  }

  def advise(problemState: ProblemState, pos: Int): Int = arity * arity

  def init(ps: ProblemState): Outcome = {
    val startDomains = ps.doms(s)
    begin = startDomains.map(_.head).min
    val end = (startDomains, ps.doms(d)).zipped.map((s, d) => s.last + d.last).max
    profile = new Array[Int](end - begin + 1)
    ps
  }

  private def buildProfile(ps: ProblemState): Outcome = {
    util.Arrays.fill(profile, 0)
    var i = s.length - 1
    var state = ps
    var bound = state.dom(b).head
    while (i >= 0) {
      val sDom = state.dom(s(i))
      val dBound = state.dom(d(i)).head
      val rBound = state.dom(r(i)).head

      // partie obligatoire entre debut au plus tard et fin au plus tôt
      for (i <- sDom.last until (sDom.head + dBound)) {
        profile(i - begin) += rBound

        if (profile(i - begin) > bound) {
          bound = profile(i - begin)
          state.removeUntil(b, bound) match {
            case c: Contradiction => return c
            case ns: ProblemState => state = ns
          }
        }
      }
      i -= 1
    }
    state
  }

  private def filter(state: ProblemState, bound: Int, i: Int): Outcome = {
    val sDom = state.dom(s(i))
    val dBound = state.dom(this.d(i)).head
    val rBound = state.dom(r(i)).head

    // Remove current task from profile
    for (i <- sDom.last until (sDom.head + dBound)) {
      profile(i - begin) -= rBound
    }

    //println(profile.mkString(" ") + " <= " + bound)

    // Sweep left
    var min = sDom.head
    var d = 0
    while (d < dBound) {
      if (profile(min + d - begin) + rBound > bound) {
        sDom.nextOption(min + d) match {
          case Some(v) => min = v
          case None => return Contradiction(s(i))
        }
        d = 0
      } else {
        d += 1
      }
    }

    // Sweep right
    var max = sDom.last + dBound - 1
    d = 0
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

    state.updateDomNonEmpty(s(i), filtered)
      .fold(filtered.last until (filtered.head + dBound)) { (state, i) =>
        profile(i - begin) += rBound
        if (profile(i - begin) > minBound) {
          minBound = profile(i - begin)
          state.removeUntil(b, profile(i - begin))
        } else {
          state
        }
      }
  }

  override def revise(ps: ProblemState, mod:BitVector): Outcome = {
    buildProfile(ps)
      .andThen { ps =>
        fixPoint(ps, s.indices, { (ps, i) => filter(ps, ps.dom(b).last, i) })
      }
  }

  def simpleEvaluation: Int = 3

}