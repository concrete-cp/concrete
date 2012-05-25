package cspfj.heuristic;

import java.util.Comparator
import cspfj.Variable
import cspfj.Problem
import scala.math.Ordering.DoubleOrdering

trait VariableHeuristic extends Ordering[Variable] {
  def problem: Problem

  def select: Option[Variable] =
    select(problem.variables filter { _.dom.size > 1 })

  def select(coll: List[Variable]) = {
//    for (v <- coll) {
//      println(v + " : " + score(v))
//    }

    if (coll.isEmpty) None
    else Some(coll.maxBy(score))
  }

  def score(variable: Variable): Double

  def compare(v1: Variable, v2: Variable) = {
    //println(v1 + ", " + v2 + " : " + score(v1).compare(score(v2)))
    score(v1).compare(score(v2))
  }

}
