package concrete.heuristic.value;

import concrete.Variable
import concrete.Problem
import concrete.Domain
import concrete.ParameterManager
import concrete.heuristic.value.ValueHeuristic
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object BestValue {

  var best: Array[Int] = _ // = new HashMap[Variable, Int]

  def newSolution(sol: Map[Variable, Any]): Unit = {
    for (best <- Option(this.best); (variable, value: Int) <- sol) {
      best(variable.id) = value
    }
  }

}

final class BestValue(fallback: ValueHeuristic) extends ValueHeuristic {

  def this(params: ParameterManager) = this{
    val valueHeuristicClass: Class[_ <: ValueHeuristic] =
      params.classInPackage("bestvalue.fallback", "concrete.heuristic.value", classOf[RandomBound])

    valueHeuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params)
  }

  override def toString = "best";

  def compute(p: Problem) {
    BestValue.best = p.variables.map(v => fallback.selectIndex(v, v.initDomain))
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, domain: Domain) = {
    val value = BestValue.best(variable.id)
    if (domain.present(value)) {
      value
    } else {
      fallback.selectIndex(variable, domain)
    }
  }

  def shouldRestart = false
}
