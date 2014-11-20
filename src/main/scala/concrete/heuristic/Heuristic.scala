package concrete.heuristic;

import concrete.Pair
import concrete.Problem;
import concrete.ProblemState

trait Heuristic {
  def selectPair(problem: Problem, state: ProblemState): Option[Pair]
  def compute(problem: Problem): Unit
}
