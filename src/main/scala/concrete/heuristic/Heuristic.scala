package concrete.heuristic;

import concrete.Pair;
import concrete.Problem;

trait Heuristic {
  def selectPair(problem: Problem): Option[Pair]
  def compute(): Unit
}
