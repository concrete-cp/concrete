package concrete.heuristic;

import concrete.Domain
import concrete.ParameterManager
import concrete.ProblemState
import concrete.Variable

final class Brelaz(params: ParameterManager, decisionVariables: Array[Variable]) extends VariableHeuristic(params, decisionVariables) {

  def compare(v1: Variable, d1: Domain, v2: Variable, d2: Domain, state: ProblemState) = {
    val domComp = d1.size.compareTo(d2.size)
    if (domComp == 0) {
      v1.getDDegEntailed(state).compareTo(v2.getDDegEntailed(state))
    } else {
      domComp
    }
  }

  override def toString = "brelaz"

}
