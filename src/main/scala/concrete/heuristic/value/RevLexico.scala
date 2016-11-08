package concrete.heuristic.value

import concrete.Variable
import concrete.Problem
import concrete.Domain
import concrete.ParameterManager
import concrete.heuristic.value.ValueHeuristic

final class RevLexico(pm: ParameterManager) extends ValueHeuristic {

  def score(variable: Variable, dom: Domain, value: Int) = value

  override def toString = "lexico";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, dom: Domain) = dom.last
  def shouldRestart = false
}
