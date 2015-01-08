package concrete.heuristic;

import concrete.constraint.Constraint
import concrete.Problem
import concrete.Variable
import concrete.ParameterManager
import concrete.ProblemState

final class DDegOnDom(params: ParameterManager) extends VariableHeuristic(params) {

  def score(variable: Variable, state: ProblemState) =
    variable.getDDegEntailed(state).toDouble / state.dom(variable).size

  override def toString = "max-ddeg/dom"

}
