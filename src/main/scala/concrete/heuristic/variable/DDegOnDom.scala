package concrete
package heuristic
package variable

class DDegOnDom(params: ParameterManager, decisionVariables: Array[Variable]) extends VariableHeuristic(params, decisionVariables) {

  def score(variable: Variable, dom: Domain, state: ProblemState) =
    variable.getDDegEntailed(state).toDouble / dom.size

  override def toString = "max-ddeg/dom"

}
