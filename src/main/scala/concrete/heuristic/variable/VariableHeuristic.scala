package concrete.heuristic.variable

import java.util.EventObject

import concrete.{MAC, ProblemState, Variable}

abstract class VariableHeuristic {
  def pool: Seq[Variable]

  def select(state: ProblemState, candidates: Seq[Variable]): Option[Variable]

  def compute(s: MAC, ps: ProblemState): ProblemState

  def shouldRestart: Boolean

  def event(e: EventObject): Unit
}
