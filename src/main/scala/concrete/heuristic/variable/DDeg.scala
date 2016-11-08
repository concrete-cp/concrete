package concrete.heuristic.variable

import concrete.Domain
import concrete.ParameterManager
import concrete.ProblemState
import concrete.Variable
import concrete.heuristic.variable.ScoredVariableHeuristic

final class DDeg(params: ParameterManager, decisionVariables: Array[Variable]) extends ScoredVariableHeuristic(params, decisionVariables) {

  def score(variable: Variable, dom: Domain, state: ProblemState) =
    variable.getDDegEntailed(state).toDouble

  override def toString = "max-ddeg"

}
