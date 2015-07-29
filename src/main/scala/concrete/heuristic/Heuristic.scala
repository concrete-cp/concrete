package concrete.heuristic;

import concrete.Pair
import concrete.Problem;
import concrete.ProblemState

trait Heuristic {
  def selectPair(state: ProblemState): Option[Pair]
  def shouldRestart: Boolean
}
