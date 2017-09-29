package concrete.heuristic.value

import concrete._

final class SmartBound(pm: ParameterManager) extends ValueHeuristic {

  override def toString = "smart-bound"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def selectIndex(variable: Variable, dom: Domain): Int = {
    if (dom.isInstanceOf[BooleanDomain]) dom.last else dom.head
  }

  def shouldRestart = false

}
