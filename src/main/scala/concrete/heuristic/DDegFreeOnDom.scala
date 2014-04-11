package concrete.heuristic;

import scala.collection.JavaConversions
import concrete.constraint.Constraint
import concrete.Problem
import concrete.Variable
import scala.annotation.tailrec
import concrete.ParameterManager

final class DDegFreeOnDom(params: ParameterManager) extends VariableHeuristic(params) {

  def score(variable: Variable) =
    variable.getDDegFree.toDouble / variable.dom.size

  override def toString = "max-ddeg-free/dom"

}
