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

  var begin: Int = _
  var end: Int = _
  var profile: Array[Int] = _

  def advise(problemState: ProblemState, pos: Int): Int = arity * arity
  def check(tuple: Array[Int]): Boolean = ???
  def init(ps: ProblemState): Outcome = {
    val startDomains = ps.doms(s)
    begin = startDomains.map(_.head).min
    end = (startDomains, ps.doms(d)).zipped.map((s, d) => s.last + d.last).max
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
            case Contradiction => return Contradiction
            case ns => state = ns.toState
          }
        }
      }
      i -= 1
    }
    state
  }

  def filter(state: ProblemState, bound: Int, i: Int): Outcome = {
    val sDom = state.dom(s(i))
    val dBound = state.dom(this.d(i)).head
    val rBound = state.dom(r(i)).head

    // Remove current task from profile
    for (i <- sDom.last until (sDom.head + dBound)) {
      profile(i - begin) -= rBound
    }

    // Sweep left
    var min = sDom.head
    var d = 0
    while (d < dBound) {
      if (profile(min + d - begin) + rBound > bound) {
        min += d + 1
        if (min > sDom.last) {
          return Contradiction
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
      Contradiction
    } else {

      //require(filtered.nonEmpty, (profile.slice(sDom.head, sDom.last + dBound).mkString, sDom, dBound, rBound, bound, min, max - dBound))

      for (i <- filtered.last until (filtered.head + dBound)) {
        profile(i - begin) += rBound
      }

      state.updateDomNonEmpty(s(i), filtered)
    }
  }

  //  private def fixPoint(ps: ProblemState, bound: Int): Outcome = {
  //    var lastModified = 0
  //    var i = 0
  //    var state = ps
  //    do {
  //      val ns = filter(state, bound, i)
  //      if (ns eq Contradiction) {
  //        return Contradiction
  //      } else if (ns ne state) {
  //        state = ns.toState
  //        lastModified = i
  //      }
  //
  //      i += 1
  //      if (i >= s.length) i = 0
  //    } while (i != lastModified)
  //    state
  //  }

  override def revise(ps: ProblemState): Outcome = {

    buildProfile(ps)
      .andThen { ps =>
        fixPoint(ps, 0 until s.length, filter(_, ps.dom(b).last, _))
      }

  }

  def simpleEvaluation: Int = 3

}