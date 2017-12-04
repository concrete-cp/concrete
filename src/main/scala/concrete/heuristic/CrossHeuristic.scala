package concrete
package heuristic

import java.util.EventObject

import concrete.heuristic.value.{BranchHeuristic, RandomValHeuristic, RandomValue}
import concrete.heuristic.variable._

import scala.util.{Random, Try}

object CrossHeuristic {
  def apply(params: ParameterManager, decisionVariables: Seq[Variable], rand: Random): Try[CrossHeuristic] = {
    for (varH <- defaultVar(params, decisionVariables, rand); valH <- defaultVal(params, rand)) yield {
      CrossHeuristic(varH, valH)
    }
  }

  def defaultVar(params: ParameterManager, decisionVariables: Seq[Variable], rand: Random): Try[VariableHeuristic] = {

    for {
      tieBreaker <- Try {
        params.getRaw("heuristic.variable.tieBreaker").map {
          case h: VariableHeuristic => h
          case "rand" => new RandomVar(decisionVariables, rand)
          case "lex" => new LexVar(decisionVariables)
        }.getOrElse(new RandomVar(decisionVariables, rand))
      }


      variableHeuristicClass <- Try(
        params.classInPackage("heuristic.variable", "concrete.heuristic.variable", classOf[variable.WDegOnDom]))

      vh <- VariableHeuristic(variableHeuristicClass, tieBreaker, decisionVariables, rand)
    } yield {
      val randomDiv = params.getOrElse("heuristic.variable.randomDiv", 0.0)

      if (randomDiv > 0) {
        new RandomDiv(vh, randomDiv, rand)
      } else {
        vh
      }
    }
  }

  def defaultVal(params: ParameterManager, rand: Random): Try[BranchHeuristic] = {
    for (heuristic <- BranchHeuristic(params, rand)) yield {
      val randomDiv = params.getOrElse("heuristic.value.randomDiv", 0.0)

      if (randomDiv > 0) {
        new RandomValHeuristic(Seq(heuristic, new RandomValue(rand)), Seq(1 - randomDiv, randomDiv), rand)
      } else {
        heuristic
      }
    }
  }

  def apply(varH: VariableHeuristic, valH: BranchHeuristic): CrossHeuristic =
    CrossHeuristic(varH, valH, varH.shouldRestart || valH.shouldRestart)
}

final case class CrossHeuristic(
                                 variableHeuristic: VariableHeuristic,
                                 valueHeuristic: BranchHeuristic,
                                 shouldRestart: Boolean) extends Heuristic {

  def branch(state: ProblemState, candidates: Seq[Variable]): Option[(Decision, Decision)] = {
    variableHeuristic.select(state, candidates).map { v =>
      assert(state.dom(v).size > 1, s"$variableHeuristic selected a singleton variable ${v.toString(state)}")
      valueHeuristic.branch(v, state.dom(v), state)
    }
  }

  def compute(solver: MAC, state: ProblemState): ProblemState = {
    // logger.fine("Initializing heuristics");
    val ps = variableHeuristic.compute(solver, state)
    valueHeuristic.compute(solver, ps)
  }

  override def toString = s"Crossed ($variableHeuristic, $valueHeuristic)"

  def decisionVariables: Seq[Variable] = variableHeuristic.pool

  def event[S <: Outcome](event: EventObject, ps: S): S = {
    valueHeuristic.event(event, variableHeuristic.event(event, ps))
  }
}
