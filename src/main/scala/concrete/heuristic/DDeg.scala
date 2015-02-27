package concrete.heuristic;

import concrete.constraint.Constraint
import concrete.Problem
import concrete.Variable
import concrete.ParameterManager
import concrete.ProblemState
import concrete.Domain

final class DDeg(params: ParameterManager) extends ScoredVariableHeuristic(params) {

  def score(variable: Variable, dom: Domain, state: ProblemState) =
    variable.getDDegEntailed(state).toDouble

  override def toString = "max-ddeg"

}
