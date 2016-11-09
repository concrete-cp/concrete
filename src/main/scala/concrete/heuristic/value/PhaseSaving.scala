package concrete.heuristic.value;

import concrete.Variable
import concrete.Problem
import concrete.Domain
import concrete.ParameterManager
import concrete.heuristic.value.ValueHeuristic
import scala.collection.mutable.HashMap

final class PhaseSaving(heuristic: ValueHeuristic) extends ValueHeuristic {

  private val previous = new HashMap[Variable, Int]

  def this(params: ParameterManager) = this{
    val valueHeuristicClass: Class[_ <: ValueHeuristic] =
      params.classInPackage("phasesaving.heuristic", "concrete.heuristic.value", classOf[RandomBound])
    valueHeuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params)
  }

  override def toString = "best";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, domain: Domain) = {
    previous.get(variable).filter(domain.present).getOrElse {
      val selected = heuristic.selectIndex(variable, domain)
      previous(variable) = selected
      selected
    }
  }

  def shouldRestart = false
}
