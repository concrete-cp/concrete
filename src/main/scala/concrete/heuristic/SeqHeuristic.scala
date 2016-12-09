package concrete.heuristic;

import concrete.ProblemState
import concrete.Problem
import concrete.Solver
import concrete.MAC

final class SeqHeuristic(heuristics: List[Heuristic]) extends Heuristic {

  def branch(state: ProblemState) = {
    heuristics.iterator.map(_.branch(state)).collectFirst {
      case Some(branching) => branching
    }
  }

  override def toString = heuristics.toString

  def shouldRestart = heuristics.head.shouldRestart

  def decisionVariables = heuristics.flatMap(_.decisionVariables).distinct

  def compute(p: Problem) = heuristics.foreach(_.compute(p))

  def applyListeners(s: MAC): Unit = heuristics.foreach(_.applyListeners(s))

}
