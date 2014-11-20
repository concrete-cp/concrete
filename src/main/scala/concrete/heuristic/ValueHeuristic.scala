package concrete.heuristic;

import concrete.Variable
import concrete.Problem
import concrete.Domain

trait ValueHeuristic {
  def selectIndex(variable: Variable, domain: Domain): Int

  def score(variable: Variable, domain: Domain, value: Int): Double

  def compute(problem: Problem)
}
