package concrete.heuristic;

import concrete.constraint.Constraint
import concrete.Problem
import concrete.Variable
import concrete.ParameterManager
import concrete.ProblemState
import concrete.Domain

final class DDeg(params: ParameterManager, decisionVariables: List[Variable]) extends ScoredVariableHeuristic(params, decisionVariables) {

  def score(variable: Variable, dom: Domain, state: ProblemState) =
    variable.getDDegEntailed(state).toDouble

  override def toString = "max-ddeg"

}
