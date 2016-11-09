package concrete.heuristic.value;

import concrete.Variable
import concrete.Problem
import concrete.Domain
import concrete.ParameterManager
import concrete.heuristic.value.ValueHeuristic

object BestValue {
  var newSolution: Map[Variable, Any] => Unit = null
}

final class BestValue(params: ParameterManager) extends ValueHeuristic {

  private var best: Map[Variable, Any] = Map.empty

  require(BestValue.newSolution == null)
  BestValue.newSolution = { sol => best = sol }

  val fallback = {
    val valueHeuristicClass: Class[_ <: ValueHeuristic] =
      params.classInPackage("bestvalue.fallback", "concrete.heuristic.value", classOf[RandomBound])

    valueHeuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params)
  }

  override def toString = "best";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, domain: Domain) = {
    best.get(variable).collect { case i: Int if domain.present(i) => i }.getOrElse(fallback.selectIndex(variable, domain))
  }

  def shouldRestart = false
}
