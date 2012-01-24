package cspfj.heuristic;

import scala.collection.JavaConversions;
import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

final class DDegOnDom(val problem: Problem) extends VariableHeuristic with RandomBreak {

  def score(variable: Variable) =
    dDeg(variable).toDouble / variable.dom.size

  private def dDeg(variable: Variable) =
    variable.constraints.iterator.filter(!_.isEntailed).size

  override def toString =
    "max-ddeg/dom"

}
