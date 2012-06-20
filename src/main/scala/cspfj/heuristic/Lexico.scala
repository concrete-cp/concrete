package cspfj.heuristic;

import cspfj.Variable;

final class Lexico extends ValueHeuristic {

  def score(variable: Variable, index: Int) = -index

  override def toString = "lexico";

  def compute() {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable) = variable.dom.last

}
