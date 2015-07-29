package concrete.heuristic;

import scala.collection.JavaConversions
import concrete.constraint.Constraint
import concrete.Problem
import concrete.Variable
import scala.annotation.tailrec
import concrete.ParameterManager
import concrete.ProblemState
import concrete.Domain

final class DDegFreeOnDom(params: ParameterManager, decisionVariables: List[Variable]) extends ScoredVariableHeuristic(params, decisionVariables) {

  def score(variable: Variable, dom: Domain, state: ProblemState) =
    variable.getDDegFree(state).toDouble / dom.size

  override def toString = "max-ddeg-free/dom"

}
