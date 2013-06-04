package concrete.heuristic;

import concrete.Variable;
import concrete.Problem

final class Lexico extends ValueHeuristic {

  def score(variable: Variable, index: Int) = -index

  override def toString = "lexico";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable) = variable.dom.first

}
