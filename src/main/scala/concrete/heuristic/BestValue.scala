package concrete.heuristic;

import concrete.Variable
import concrete.Problem
import concrete.Domain
import cspom.StatisticsManager
import concrete.ParameterManager

object BestValue {
  private var best: Map[Variable, Any] = Map.empty

  def newSolution(sol: Map[Variable, Any]): Unit = best = sol
}

final class BestValue(params: ParameterManager) extends ValueHeuristic {

  val fallback = {
    val valueHeuristicClass: Class[_ <: ValueHeuristic] =
      params.getOrElse("bestvalue.fallback", classOf[RandomValue])

    valueHeuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params)
  }

  override def toString = "best";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, domain: Domain) = {
    BestValue.best.get(variable).collect { case i: Int if domain.present(i) => i }.getOrElse(fallback.selectIndex(variable, domain))
  }

  def shouldRestart = false
}
