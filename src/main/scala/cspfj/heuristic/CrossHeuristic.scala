package cspfj.heuristic;

import java.lang.reflect.InvocationTargetException;

import cspfj.Pair;
import cspfj.ParameterManager;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Parameter;

object CrossHeuristic {
  @Parameter("heuristic.variable")
  var variableHeuristicClass: Class[_ <: VariableHeuristic] = classOf[WDegOnDom]

  @Parameter("heuristic.value")
  var valueHeuristicClass: Class[_ <: ValueHeuristic] = classOf[Lexico]

  ParameterManager.register(classOf[CrossHeuristic]);
}

final class CrossHeuristic(problem: Problem) extends Heuristic {

  val variableHeuristic = CrossHeuristic.variableHeuristicClass.getConstructor(classOf[Problem]).newInstance(problem)

  val valueHeuristic = CrossHeuristic.valueHeuristicClass.getConstructor().newInstance()

  def selectPair(problem: Problem) = {
    variableHeuristic.select match {
      case None => None
      case Some(v) => Some(Pair(v, valueHeuristic.selectIndex(v)))
    }
  }

  def compute() {
    // logger.fine("Initializing heuristics");
    valueHeuristic.compute();
  }

  override def toString =
    "Crossed " + variableHeuristic + ", " + valueHeuristic;

}
