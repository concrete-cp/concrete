package concrete
package heuristic;

import concrete.ParameterManager
import concrete.Problem
import concrete.ProblemState
import concrete.Variable
import concrete.heuristic.variable.VariableHeuristic
import concrete.heuristic.value.BranchHeuristic
import concrete.heuristic.value.BestValue
import concrete.MAC
import java.util.EventObject

object CrossHeuristic {
  def apply(params: ParameterManager, decisionVariables: Array[Variable]): CrossHeuristic = {
    val varH = defaultVar(params, decisionVariables)
    val valH = defaultVal(params)

    CrossHeuristic(varH, valH)
  }

  def defaultVar(params: ParameterManager, decisionVariables: Array[Variable]) = {
    val variableHeuristicClass: Class[_ <: VariableHeuristic] =
      params.classInPackage("heuristic.variable", "concrete.heuristic.variable", classOf[variable.WDegOnDom])

    variableHeuristicClass
      .getConstructor(classOf[ParameterManager], classOf[Array[Variable]])
      .newInstance(params, decisionVariables)
  }

  def defaultVal(params: ParameterManager) = {
    val valueHeuristicClass: Class[_ <: BranchHeuristic] =
      params.classInPackage("heuristic.value", "concrete.heuristic.value", classOf[BestValue])

    valueHeuristicClass
      .getConstructor(classOf[ParameterManager])
      .newInstance(params)

  }

  def apply(varH: VariableHeuristic, valH: BranchHeuristic): CrossHeuristic =
    CrossHeuristic(varH, valH, varH.shouldRestart || valH.shouldRestart)
}

final case class CrossHeuristic(
    val variableHeuristic: VariableHeuristic,
    val valueHeuristic: BranchHeuristic,
    val shouldRestart: Boolean) extends Heuristic {

  def branch(state: ProblemState) = {
    variableHeuristic.select(state).map { v =>
      assert(state.dom(v).size > 1, s"$variableHeuristic selected a singleton variable ${v.toString(state)}")
      valueHeuristic.branch(v, state.dom(v), state)
    }
  }

  def compute(problem: Problem) {
    // logger.fine("Initializing heuristics");
    valueHeuristic.compute(problem: Problem);
  }

  override def toString =
    "Crossed (" + variableHeuristic + ", " + valueHeuristic + ")";

  def decisionVariables = variableHeuristic.decisionVariables

  def event(event: EventObject) = {
    variableHeuristic.event(event)
    valueHeuristic.event(event)
  }
}
