package concrete.heuristic;

import concrete.Domain
import concrete.ParameterManager
import concrete.ProblemState
import concrete.Variable

final class DDegFreeOnDom(params: ParameterManager, decisionVariables: Array[Variable]) extends ScoredVariableHeuristic(params, decisionVariables) {

  def score(variable: Variable, dom: Domain, state: ProblemState) =
    variable.getDDegFree(state).toDouble / dom.size

  override def toString = "max-ddeg-free/dom"

}
