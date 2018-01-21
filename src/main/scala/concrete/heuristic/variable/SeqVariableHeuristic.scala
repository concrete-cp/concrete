package concrete.heuristic.variable

import java.util.EventObject

import concrete.{MAC, Outcome, ProblemState, Variable}

object SeqVariableHeuristic {
  def apply(hs: Seq[VariableHeuristic]): VariableHeuristic = hs match {
    case Seq(h) => h
    case _ => new SeqVariableHeuristic(hs)
  }
}

class SeqVariableHeuristic(
                            val heuristics: Seq[VariableHeuristic]
                          ) extends VariableHeuristic {
  def pool: Seq[Variable] = heuristics.flatMap(_.pool).distinct

  def select(state: ProblemState, candidates: Seq[Variable]): Seq[Variable] = {
    heuristics.iterator.map { h =>
      h.select(state, candidates)
    }.find(_.nonEmpty).getOrElse(Seq())
  }

  def compute(s: MAC, ps: ProblemState): ProblemState = heuristics.foldRight(ps)(_.compute(s, _))

  def shouldRestart: Boolean = heuristics.exists(_.shouldRestart)

  override def event[S <: Outcome](event: EventObject, ps: S): S = heuristics.foldLeft(ps) {
    case (s, h) => h.event(event, s)
  }

  override def toString: String = heuristics.mkString("SeqVH(", ", ", ")")
}
