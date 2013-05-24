package concrete.heuristic;

import java.lang.reflect.InvocationTargetException;

import concrete.Pair;
import concrete.ParameterManager;
import concrete.Problem;
import concrete.Variable;
import concrete.Parameter;

object CrossHeuristic {
  @Parameter("heuristic.variable")
  var variableHeuristicClass: Class[_ <: VariableHeuristic] = classOf[WDegOnDom]

  @Parameter("heuristic.value")
  var valueHeuristicClass: Class[_ <: ValueHeuristic] = classOf[Lexico]

  ParameterManager.register(this);
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
