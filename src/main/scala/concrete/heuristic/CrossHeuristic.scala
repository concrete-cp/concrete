package concrete.heuristic;

import java.lang.reflect.InvocationTargetException
import concrete.Pair
import concrete.ParameterManager
import concrete.Problem
import concrete.Variable;
import concrete.ProblemState

final class CrossHeuristic(params: ParameterManager) extends Heuristic {

  private val variableHeuristicClass: Class[_ <: VariableHeuristic] =
    params.getOrElse("heuristic.variable", classOf[WDegOnDom])

  private val valueHeuristicClass: Class[_ <: ValueHeuristic] =
    params.getOrElse("heuristic.value", classOf[Lexico])

  val variableHeuristic = variableHeuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params)

  val valueHeuristic = valueHeuristicClass.getConstructor().newInstance()

  def selectPair(problem: Problem, state: ProblemState) = {
    variableHeuristic.select(problem, state).map(v => Pair(v, valueHeuristic.selectIndex(v, state.dom(v))))
  }

  def compute(problem: Problem) {
    // logger.fine("Initializing heuristics");
    valueHeuristic.compute(problem: Problem);
  }

  override def toString =
    "Crossed " + variableHeuristic + ", " + valueHeuristic;

}
