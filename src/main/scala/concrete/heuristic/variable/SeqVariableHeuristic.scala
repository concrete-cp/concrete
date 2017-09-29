package concrete.heuristic.variable

import java.util.EventObject

import concrete.{MAC, ProblemState, Variable}

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

  def select(state: ProblemState, candidates: Seq[Variable]): Option[Variable] = {
    heuristics.iterator.map { h =>
      h.select(state, candidates)
    }.collectFirst {
      case Some(branching) => branching
    }
  }

  def compute(s: MAC, ps: ProblemState): ProblemState = heuristics.foldRight(ps)(_.compute(s, _))

  def shouldRestart: Boolean = heuristics.exists(_.shouldRestart)

  override def event(event: EventObject): Unit = heuristics.foreach(_.event(event))

  override def toString = heuristics.mkString("SeqVH(", ", ", ")")
}
