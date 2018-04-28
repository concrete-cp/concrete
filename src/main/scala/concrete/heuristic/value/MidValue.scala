package concrete
package heuristic
package value

import cspom.UNSATException

final class MidValue() extends ValueSelector {

  override def toString = "middle"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def select(ps: ProblemState, variable: Variable, candidates: Domain): (Outcome, Domain) = {
    val middle = (candidates.head + candidates.last) / 2
    val actualmid = (candidates.prevOption(middle), candidates.nextOption(middle)) match {
      case (None, None) => throw new UNSATException(s"$candidates is empty")
      case (Some(b), None) => b
      case (None, Some(a)) => a
      case (Some(b), Some(a)) =>
        val da = math.abs(a - middle)
        val db = math.abs(b - middle)

        if (da < db) {
          a
        } else {
          b
        }
    }
    (ps, Singleton(actualmid))
  }

  def shouldRestart = false
}
