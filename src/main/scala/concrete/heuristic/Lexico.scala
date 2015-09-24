package concrete.heuristic;

import concrete.Variable
import concrete.Problem
import concrete.Domain
import concrete.ProblemState
import concrete.ParameterManager

final class Lexico(pm: ParameterManager) extends ValueHeuristic {

  override def toString = "lexico";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, dom: Domain) = dom.head

  def shouldRestart = false

}
