package cspfj.heuristic;

import java.util.Comparator
import cspfj.Variable
import cspfj.Problem
import scala.math.Ordering.DoubleOrdering

trait VariableHeuristic extends Ordering[Variable] {
  def problem: Problem

  def select: Option[Variable] =
    select(problem.variables filter { _.dom.size > 1 })

  def select(coll: List[Variable]) =
    if (coll.isEmpty) None
    else Some(coll.reduceLeft(
      (x, y) => if (compare(x, y) > 0) x else y))

  def score(variable: Variable): Double

  def compare(v1: Variable, v2: Variable) =
    score(v1).compare(score(v2))

}
