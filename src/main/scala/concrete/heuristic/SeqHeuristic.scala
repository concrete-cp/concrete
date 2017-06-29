package concrete
package heuristic

import java.util.EventObject

final class SeqHeuristic(heuristics: List[Heuristic]) extends Heuristic {

  def branch(state: ProblemState) = {
    heuristics.iterator.map(_.branch(state)).collectFirst {
      case Some(branching) => branching
    }
  }

  override def toString = heuristics.toString

  def shouldRestart = heuristics.head.shouldRestart

  def decisionVariables = heuristics.flatMap(_.decisionVariables).distinct

  def compute(p: Problem, state: ProblemState): ProblemState =
    heuristics.foldLeft(state) { case (s, h) => h.compute(p, s) }

  def event(event: EventObject): Unit = heuristics.foreach(_.event(event))

}
