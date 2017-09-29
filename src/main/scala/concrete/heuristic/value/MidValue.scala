package concrete
package heuristic
package value

import cspom.UNSATException

final class MidValue(pm: ParameterManager) extends ValueHeuristic {

  override def toString = "median"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def selectIndex(variable: Variable, domain: Domain): Int = {
    val middle = (domain.head + domain.last) / 2
    (domain.prevOption(middle), domain.nextOption(middle)) match {
      case (None, None)    => throw new UNSATException(s"$domain is empty")
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

  }

  def shouldRestart = false
}
