package concrete.heuristic;

import concrete.Domain
import concrete.ParameterManager
import concrete.Problem
import concrete.Variable

final class Lexico(pm: ParameterManager) extends ValueHeuristic {

  override def toString = "lexico";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, dom: Domain) = dom.head

  def shouldRestart = false

}
