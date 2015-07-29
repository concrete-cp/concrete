package concrete.heuristic;

import concrete.Variable
import concrete.Problem
import concrete.Domain

final class RevLexico extends ValueHeuristic {

  def score(variable: Variable, dom: Domain, value: Int) = value

  override def toString = "lexico";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, dom: Domain) = dom.last
  def shouldRestart = false
}
