package concrete
package heuristic

import java.util.EventObject

import concrete.heuristic.variable.VariableHeuristic

object SeqHeuristic {
  def extractVariableHeuristics(h: Seq[Heuristic]): Seq[VariableHeuristic] = {
    h.flatMap {
      case CrossHeuristic(vh, _, _) => vh
      case sh: SeqHeuristic => extractVariableHeuristics(sh.heuristics)
      case _ => Seq()
    }
  }

  def apply(hs: Seq[Heuristic]): Heuristic = hs match {
    case Seq(h) => h
    case _ => new SeqHeuristic(hs)
  }
}

final class SeqHeuristic(val heuristics: Seq[Heuristic]) extends Heuristic {

  def branch(state: ProblemState, candidates: Seq[Variable]): Option[(Decision, Decision)] = {
    heuristics.iterator.map { h =>
      h.branch(state, candidates)
    }.collectFirst {
      case Some(branching) => branching
    }
  }

  override def toString: String = heuristics.mkString("SeqHeuristic(", ", ", ")")

  def shouldRestart: Boolean = heuristics.head.shouldRestart

  def decisionVariables: Seq[Variable] = heuristics.flatMap(_.decisionVariables).distinct

  def compute(s: MAC, state: ProblemState): ProblemState =
    heuristics.foldLeft(state) { case (ps, h) => h.compute(s, ps) }

  def event[S <: Outcome](event: EventObject, ps: S): S =
    heuristics.foldLeft(ps) { case (p, h) => h.event(event, p) }

}
