package concrete
package heuristic
package value

import java.util.EventObject

import com.typesafe.scalalogging.LazyLogging

final class BestValue(fallback: ValueHeuristic) extends ValueHeuristic with LazyLogging {

  def this(params: ParameterManager) = this{
    val valueHeuristicClass: Class[_ <: ValueHeuristic] =
      params.classInPackage("bestvalue.fallback", "concrete.heuristic.value", classOf[RevLexico])

    valueHeuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params)
  }

  override def toString = "best"

  private var best: Map[Variable, Int] = Map()

  def compute(p: Problem) {
    require(p.variables.zipWithIndex.forall { case (v, i) => v.id == i })
    fallback.compute(p)
  }

  override def selectIndex(variable: Variable, domain: Domain) = {
    best.get(variable).filter(domain.present)
      .getOrElse {
        logger.debug(s"not present in $variable $domain, fallback")
        fallback.selectIndex(variable, domain)
      }
  }

  def shouldRestart = false

  override def event(event: EventObject): Unit = event match {
    case NewSolutionEvent(sol) =>
      logger.info(s"New solution")
      best = sol
    case _ =>
  }

}
