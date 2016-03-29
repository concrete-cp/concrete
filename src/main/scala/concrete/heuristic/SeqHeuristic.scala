package concrete.heuristic;

import concrete.ProblemState

final class SeqHeuristic(heuristics: List[Heuristic]) extends Heuristic {

  def branch(state: ProblemState) = {
    heuristics.iterator.map(_.branch(state)).collectFirst {
      case Some(branching) => branching
    }
  }

  override def toString = heuristics.toString

  def shouldRestart = heuristics.exists(_.shouldRestart)

}
