package concrete.heuristic;

import scala.collection.JavaConversions
import concrete.constraint.Constraint
import concrete.Problem
import concrete.Variable
import scala.annotation.tailrec
import concrete.ParameterManager
import concrete.ProblemState

final class DDegFreeOnDom(params: ParameterManager) extends VariableHeuristic(params) {

  def score(variable: Variable, state: ProblemState) =
    variable.getDDegFree(state).toDouble / state(variable).size

  override def toString = "max-ddeg-free/dom"

}
