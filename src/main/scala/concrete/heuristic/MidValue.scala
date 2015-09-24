package concrete.heuristic;

import concrete.Variable
import concrete.Problem
import concrete.Domain
import cspom.StatisticsManager
import concrete.ParameterManager

final class MidValue(pm: ParameterManager) extends ValueHeuristic {

  def score(variable: Variable, domain: Domain, value: Int) = ((domain.head + domain.last) / 2) - value

  override def toString = "median";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, domain: Domain) = {
    val middle = (domain.head + domain.last) / 2
    val after = domain.next(middle)
    val before = domain.prev(middle)

    if (after < 0) {
      before
    } else if (before < 0) {
      after
    } else {
      val da = math.abs(after - middle)
      val db = math.abs(before - middle)

      if (da < db) {
        after
      } else {
        before
      }

    }

  }

  def shouldRestart = false
}
