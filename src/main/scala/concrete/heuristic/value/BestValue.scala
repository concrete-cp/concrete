package concrete
package heuristic
package value

import java.util.{EventObject, Random}

import com.typesafe.scalalogging.LazyLogging

final class BestValue(fallback: BranchHeuristic) extends BranchHeuristic with LazyLogging {

  private var best: Map[Variable, Int] = Map()

  def this(params: ParameterManager) = this {
    val valueHeuristicClass: Class[_ <: BranchHeuristic] =
      params.classInPackage("bestvalue.fallback", "concrete.heuristic.value", classOf[SmartBound])

    valueHeuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params)
  }

  override def toString = s"best or $fallback"

  private val rand = new Random(0)

  def compute(s: MAC, ps: ProblemState): ProblemState = {
    require(s.problem.variables.zipWithIndex.forall { case (v, i) => v.id == i })

    fallback.compute(s, ps)
  }

  def branch(variable: Variable, domain: Domain, ps: ProblemState): Branch = {
    best.get(variable).filter(_=>rand.nextBoolean()).filter(domain.present).map {
      i =>
        logger.debug(s"assigning value from best solution $i")
        assignBranch(ps, variable, domain, i)
    }
      .getOrElse {
        logger.debug(s"not present in $variable $domain, fallback")
        fallback.branch(variable, domain, ps)
      }
  }

  def shouldRestart = false

  override def event(event: EventObject): Unit

  = event match {
    case NewSolutionEvent(sol) =>
      logger.info(s"New solution")
      best = sol
    case _ =>
  }

}
