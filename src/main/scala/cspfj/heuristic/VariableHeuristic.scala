package cspfj.heuristic;

import java.util.Comparator
import cspfj.problem.Variable
import cspfj.problem.Problem
import scala.math.Ordering.DoubleOrdering

trait VariableHeuristic extends Ordering[Variable] {
  def problem: Problem

  def select: Option[Variable] =
    select(problem.variables.iterator filter { _.dom.size > 1 })

  def select(coll: Iterator[Variable]) =
    if (coll.isEmpty) None
    else Some(coll.max(this))

  def score(variable: Variable): Double

  def compare(v1: Variable, v2: Variable) =
    score(v1).compare(score(v2))

}
