package concrete
package constraint
package semantic

import java.util.Arrays

/**
 * Requires that a set of tasks given by start times s, durations d, and
 * resource requirements r, never require more than a global resource bound
 * b at any one time.
 *
 * Assumptions:
 * - forall i, d[i] >= 0 and r[i] >= 0
 */

class Cumulative(s: Array[Variable], d: Array[Variable], r: Array[Variable], b: Variable) extends Constraint(s ++ d ++ r :+ b)
    with BC {

  private var begin: Int = _
  private var profile: Array[Int] = _

  def advise(problemState: ProblemState, pos: Int): Int = arity * arity
  def check(tuple: Array[Int]): Boolean = ???
  def init(ps: ProblemState): Outcome = {
    val startDomains = ps.doms(s)
    begin = startDomains.map(_.head).min
    val end = (startDomains, ps.doms(d)).zipped.map((s, d) => s.last + d.last).max
    profile = new Array[Int](end - begin + 1)
    ps
  }

  private def buildProfile(ps: ProblemState): Outcome = {
    Arrays.fill(profile, 0)
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
        min += d + 1
        if (min > sDom.last) {
          return Contradiction(s(i))
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
        max -= d + 1
        d = 0
      } else {
        d += 1
      }
    }

    val filtered = sDom & (min, max - dBound + 1)

    if (filtered.isEmpty) {
      // Can happen if there are "holes" in the domains
      Contradiction(s(i))
    } else {

      //require(filtered.nonEmpty, (profile.slice(sDom.head, sDom.last + dBound).mkString, sDom, dBound, rBound, bound, min, max - dBound))

      for (i <- filtered.last until (filtered.head + dBound)) {
        profile(i - begin) += rBound
      }

      state.updateDomNonEmpty(s(i), filtered)
    }
  }

  override def revise(ps: ProblemState): Outcome = {

    buildProfile(ps)
      .andThen { ps =>
        fixPoint(ps, 0 until s.length, { (ps, i) => filter(ps, ps.dom(b).last, i) })
      }

  }

  def simpleEvaluation: Int = 3

}