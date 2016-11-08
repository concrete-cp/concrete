package concrete.heuristic.value

import concrete.Domain
import concrete.ParameterManager
import concrete.Problem
import concrete.Variable
import cspom.UNSATException
import concrete.heuristic.value.ValueHeuristic

final class MidValue(pm: ParameterManager) extends ValueHeuristic {

  def score(variable: Variable, domain: Domain, value: Int) = ((domain.head + domain.last) / 2) - value

  override def toString = "median";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, domain: Domain) = {
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
