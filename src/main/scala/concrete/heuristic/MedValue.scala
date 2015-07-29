package concrete.heuristic;

import concrete.Variable
import concrete.Problem
import concrete.Domain
import cspom.StatisticsManager

final class MedValue extends ValueHeuristic {

  def score(variable: Variable, domain: Domain, value: Int) = math.abs((domain.last + domain.head) / 2 - value)

  override def toString = "median";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, domain: Domain) = {
    StatisticsManager.median(domain.toStream)
  }
  def shouldRestart = false
}
