package concrete
package heuristic
package variable

import concrete.constraint.Constraint

class WArcsOnDom(params: ParameterManager, decisionVariables: Array[Variable]) extends ScoredVariableHeuristic(params, decisionVariables) {

  val arcs: Array[Array[Int]] = {
    val max = decisionVariables.map(_.id).max + 1

    Array.ofDim[Int](max, max)
  }

  def score(variable: Variable, dom: Domain, state: ProblemState) = {
    variable.getWDegEntailed(state).toDouble / dom.size
  }

  override def applyListeners(s: MAC) = s.filter.contradictionListener = Some({
    case (ctr: Constraint, cause: Option[Variable]) =>
      ctr.weight += 1
  })

}