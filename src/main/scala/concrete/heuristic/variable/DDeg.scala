package concrete
package heuristic
package variable

final class DDeg(params: ParameterManager, decisionVariables: Array[Variable]) extends VariableHeuristic(params, decisionVariables) {

  def score(variable: Variable, dom: Domain, state: ProblemState) =
    variable.getDDegEntailed(state).toDouble

  override def toString = "max-ddeg"

}
