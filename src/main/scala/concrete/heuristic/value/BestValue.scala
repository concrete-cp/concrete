package concrete.heuristic.value;

import concrete.Variable
import concrete.Problem
import concrete.Domain
import concrete.ParameterManager
import concrete.heuristic.value.ValueHeuristic
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object BestValue {

  val best = new HashMap[Variable, ListBuffer[Int]]

  def newSolution(sol: Map[Variable, Any]): Unit = {
    for ((variable, value: Int) <- sol) {
      val order = best.getOrElseUpdate(variable, new ListBuffer())

      order -= value
      value +=: order
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
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, domain: Domain) = {
    BestValue.best.getOrElse(variable, Nil)
      .headOption
      .filter(domain.present)
      .getOrElse(fallback.selectIndex(variable, domain))
  }

  def shouldRestart = false
}
