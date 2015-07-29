package concrete.heuristic;

import concrete.Variable
import concrete.Problem
import concrete.Domain

final class Lexico extends ValueHeuristic {

  def score(variable: Variable, domain: Domain, index: Int) = -index

  override def toString = "lexico";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, dom: Domain) = dom.head
  
  def shouldRestart = false

}
