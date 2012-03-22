package cspfj.heuristic;

import scala.collection.JavaConversions
import cspfj.constraint.Constraint
import cspfj.problem.Problem
import cspfj.problem.Variable;
import scala.annotation.tailrec

final class DDegOnDom(val problem: Problem) extends VariableHeuristic with RandomBreak {

  def score(variable: Variable) =
    variable.getDDeg.toDouble / variable.dom.size

  override def toString = "max-ddeg/dom"

}
