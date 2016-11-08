package concrete.heuristic.value

import concrete.Domain
import concrete.ParameterManager
import concrete.Problem
import concrete.Variable
import concrete.heuristic.value.ValueHeuristic

final class Lexico(pm: ParameterManager) extends ValueHeuristic {

  override def toString = "lexico";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, dom: Domain) = dom.head

  def shouldRestart = false

}
