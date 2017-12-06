package concrete.heuristic.value

import com.typesafe.scalalogging.LazyLogging
import concrete._

final class SmartBound() extends ValueHeuristic with LazyLogging {

  override def toString = "smart-bound"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def selectIndex(variable: Variable, dom: Domain): Int = {

    val r = if (dom.isInstanceOf[BooleanDomain]) dom.last else dom.head
    logger.info(s"Smart bound is $r")
    r
  }

  def shouldRestart = false

}
