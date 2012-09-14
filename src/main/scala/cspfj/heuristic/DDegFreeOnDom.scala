package cspfj.heuristic;

import scala.collection.JavaConversions
import cspfj.constraint.Constraint
import cspfj.Problem
import cspfj.Variable;
import scala.annotation.tailrec

final class DDegFreeOnDom(val problem: Problem) extends VariableHeuristic with RandomBreak {

  def score(variable: Variable) =
    variable.getDDegFree.toDouble / variable.dom.size

  override def toString = "max-ddeg-free/dom"

}
