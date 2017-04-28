package concrete
package heuristic
package variable

final class Brelaz(params: ParameterManager, decisionVariables: Array[Variable]) extends VariableHeuristic(params, decisionVariables) {

  private val factor = decisionVariables.map(_.initDomain.size).max

  def score(variable: Variable, dom: Domain, state: ProblemState) = {
    dom.size * factor + variable.getDDegEntailed(state)
  }

  override def toString = "brelaz"

}
