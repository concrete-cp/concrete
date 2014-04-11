package concrete.heuristic;


import concrete.constraint.Constraint
import concrete.Problem
import concrete.Variable;
import concrete.ParameterManager

final class DDegOnDom(params: ParameterManager) extends VariableHeuristic(params) {

  def score(variable: Variable) =
    variable.getDDegEntailed.toDouble / variable.dom.size

  override def toString = "max-ddeg/dom"

}
