package concrete.heuristic;

import concrete.ParameterManager
import concrete.Problem
import concrete.ProblemState
import concrete.Variable

object CrossHeuristic {
  def apply(params: ParameterManager, decisionVariables: Array[Variable]) = {
    new CrossHeuristic(params, defaultVar(params, decisionVariables), defaultVal(params))
  }

  def defaultVar(params: ParameterManager, decisionVariables: Array[Variable]) = {
    val variableHeuristicClass: Class[_ <: VariableHeuristic] =
      params.getOrElse("heuristic.variable", classOf[WDegOnDom])
    variableHeuristicClass.getConstructor(classOf[ParameterManager], classOf[Array[Variable]]).newInstance(params, decisionVariables)
  }

  def defaultVal(params: ParameterManager) = {
    val valueHeuristicClass: Class[_ <: BranchHeuristic] =
      params.getOrElse("heuristic.value", classOf[BestValue])

    valueHeuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params)

  }
}

final class CrossHeuristic(
    params: ParameterManager,
    val variableHeuristic: VariableHeuristic,
    val valueHeuristic: BranchHeuristic) extends Heuristic {

  def branch(state: ProblemState) = {
    variableHeuristic.select(state).map { v =>
      require(state.dom(v).size > 1, s"$variableHeuristic selected a singleton variable ${v.toString(state)}")
      valueHeuristic.branch(v, state.dom(v), state)
    }
  }

  def compute(problem: Problem) {
    // logger.fine("Initializing heuristics");
    valueHeuristic.compute(problem: Problem);
  }

  override def toString =
    "Crossed (" + variableHeuristic + ", " + valueHeuristic + ")";

  def shouldRestart = variableHeuristic.shouldRestart || valueHeuristic.shouldRestart
}
