package concrete
package heuristic

import java.util.EventObject

import concrete.heuristic.value.BranchHeuristic
import concrete.heuristic.variable._

import scala.util.{Random, Try}

object CrossHeuristic {
  def apply(params: ParameterManager, decisionVariables: Seq[Variable], rand: Random): Try[CrossHeuristic] = {
    for (varH <- VariableHeuristic.default(params, decisionVariables, rand); valH <- BranchHeuristic.default(params, rand)) yield {
      CrossHeuristic(varH, valH)
    }
  }

  def apply(varH: Seq[VariableHeuristic], valH: BranchHeuristic): CrossHeuristic =
    CrossHeuristic(varH, valH, varH.exists(_.shouldRestart) || valH.shouldRestart)
}

final case class CrossHeuristic(
                                 variableHeuristic: Seq[VariableHeuristic],
                                 valueHeuristic: BranchHeuristic,
                                 shouldRestart: Boolean) extends Heuristic {


  def branch(state: ProblemState, candidates: Seq[Variable]): Option[(Decision, Decision)] = {

    val it = variableHeuristic.iterator

    def selectOne(cand: Seq[Variable]): Option[Variable] = {
      if (!it.hasNext || cand.lengthCompare(1) <= 0) {
        cand.headOption
      } else {
        selectOne(it.next().select(state, cand))
      }
    }

    selectOne(candidates).map { v =>
      assert(state.dom(v).size > 1, s"$variableHeuristic selected a singleton variable ${v.toString(state)}")
      valueHeuristic.branch(v, state.dom(v), state)
    }
  }

  def compute(solver: MAC, state: ProblemState): ProblemState = {
    // logger.fine("Initializing heuristics");
    val ps = variableHeuristic.foldLeft(state)((acc, vh) => vh.compute(solver, acc))
    valueHeuristic.compute(solver, ps)
  }

  override def toString = s"Crossed ($variableHeuristic, $valueHeuristic)"

  def decisionVariables: Seq[Variable] = variableHeuristic.flatMap(_.pool).distinct

  def event[S <: Outcome](event: EventObject, ps: S): S = {
    val s = variableHeuristic.foldLeft(ps)((acc, vh) => vh.event(event, acc))
    valueHeuristic.event(event, s)
  }
}
