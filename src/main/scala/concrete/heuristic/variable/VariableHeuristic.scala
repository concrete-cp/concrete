package concrete.heuristic.variable

import java.util.EventObject

import concrete.{MAC, Outcome, ProblemState, Variable}

import scala.util.{Random, Try}

object VariableHeuristic {
  def apply(variableHeuristicClass: Class[_ <: VariableHeuristic], tieBreaker: VariableHeuristic,
            decisionVariables: Seq[Variable], rand: Random): Try[VariableHeuristic] = {

    Try {
      variableHeuristicClass.getConstructor(classOf[Seq[Variable]], classOf[VariableHeuristic])
        .newInstance(decisionVariables, tieBreaker)
    }
      .recover {
        case _: NoSuchMethodException =>
          variableHeuristicClass.getConstructor(classOf[Seq[Variable]], classOf[Random])
            .newInstance(decisionVariables, rand)
      }
      .recover {
        case _: NoSuchMethodException =>
          variableHeuristicClass.getConstructor(classOf[Seq[Variable]])
            .newInstance(decisionVariables)
      }
  }
}

abstract class VariableHeuristic {
  def pool: Seq[Variable]

  def select(state: ProblemState, candidates: Seq[Variable]): Option[Variable]

  def compute(s: MAC, ps: ProblemState): ProblemState

  def shouldRestart: Boolean

  def event[S <: Outcome](e: EventObject, state: S): S
}