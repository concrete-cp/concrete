package cspfj.heuristic;

import cspfj.Variable;

final class MedValue extends ValueHeuristic {

  def score(variable: Variable, index: Int) = -index

  override def toString = "median";

  def compute() {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable) = {
    val skip = variable.dom.size / 2
    variable.dom.indices.drop(skip).head
  }

}
