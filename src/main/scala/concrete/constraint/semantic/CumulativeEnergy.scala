package concrete
package constraint
package semantic

import bitvectors.BitVector

object Task {
  def apply(ps: ProblemState, s: Variable, d: Variable, h: Variable): Task = {
    val sDom = ps.dom(s)
    val dDom = ps.dom(d)
    Task(d, h, sDom.head, dDom.head, sDom.last + dDom.last, ps.dom(h).head)
  }
}

case class Task(d: Variable, h: Variable, slb: Int, dlb: Int, eub: Int, hlb: Int) extends Ordered[Task] {
  assert(eub > slb)

  private val coef = (dlb * hlb).toDouble / (eub - slb)

  def compare(t: Task): Int = java.lang.Double.compare(coef, t.coef)
}

/**
  * Requires that a set of tasks given by start times s, durations d, and
  * resource requirements r, never require more than a global resource bound
  * b at any one time.
  *
  * Assumptions:
  * - forall i, d[i] >= 0 and r[i] >= 0
  */

class CumulativeEnergy(s: Array[Variable], d: Array[Variable], h: Array[Variable], b: Variable) extends Constraint(s ++ d ++ h :+ b)
  with BC with CumulativeChecker with FixPoint {

  val tasks = new Array[Task](s.length)

  def nbTasks: Int = s.length

  def advise(problemState: ProblemState, pos: Int): Int = arity * arity

  def init(ps: ProblemState): Outcome = ps

  def revise(ps: ProblemState, mod: BitVector): Outcome = fixPoint(ps, shave(_))

  final def shave(ps: ProblemState): Outcome = {
    val tasks = Array.tabulate(s.length) { i =>
      Task(ps, s(i), d(i), h(i))
    }

    scala.util.Sorting.quickSort(tasks) //stableSort(order, (i: Int, j: Int) => tasks(i).coef < tasks(j).coef)

    var xMin = Int.MaxValue / 2
    var xMax = Int.MinValue / 2
    var surface = 0
    val camax = ps.dom(b).last

    ps.fold(tasks) { (ps: ProblemState, t) =>
      xMax = Math.max(xMax, t.eub)
      xMin = Math.min(xMin, t.slb)
      val xDiff = xMax - xMin
      if (xDiff >= 0) {
        val availSurf: Int = xDiff * camax - surface
        surface += t.dlb * t.hlb
        if (surface > xDiff * camax) {
          Contradiction(scope)
        } else {
          var state: Outcome = ps
          if (t.dlb > 0) state = state.removeAfter(t.h, Math.floorDiv(availSurf, t.dlb))
          if (t.hlb > 0) state = state.removeAfter(t.d, Math.floorDiv(availSurf, t.hlb))
          if (xDiff > 0) state = state.removeUntil(b, util.Math.ceilDiv(surface, xDiff))
          state
        }
      } else {
        ps
      }
    }

  }

  def simpleEvaluation: Int = 3

}