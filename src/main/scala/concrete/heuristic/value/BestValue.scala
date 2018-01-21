package concrete
package heuristic
package value

import java.util.EventObject

import com.typesafe.scalalogging.LazyLogging

import scala.util.Random

object BestValue {
  def apply(params: ParameterManager, rand: Random): BranchHeuristic = {
    val valueHeuristicClass: Class[_ <: BranchHeuristic] =
      params.classInPackage("bestvalue.fallback", "concrete.heuristic.value", classOf[Lexico])

    val fallback = BranchHeuristic(valueHeuristicClass, params, rand).get

    val bestValue = new BestValue(fallback)

    params.get[Double]("bestvalue.proba") match {
      case Some(proba) => new RandomValHeuristic(Seq(bestValue, fallback), Seq(proba, 1.0 - proba), rand)
      case None => bestValue
    }
  }
}

final class BestValue(fallback: BranchHeuristic) extends BranchHeuristic with LazyLogging {

  private var best: Map[Variable, Int] = Map()

  override def toString = s"best or $fallback"

  def compute(s: MAC, ps: ProblemState): ProblemState = {
    require(s.problem.variables.zipWithIndex.forall { case (v, i) => v.id == i })

    fallback.compute(s, ps)
  }

  def branch(variable: Variable, domain: Domain, ps: ProblemState): (Decision, Decision) = {
    best.get(variable)
      .filter(domain.present)
      .map { i =>
        logger.info(s"assigning value from best solution $i")
        assignBranch(ps, variable, i)
      }
      .getOrElse {
        logger.info(s"not present in $variable $domain, fallback")
        fallback.branch(variable, domain, ps)
      }
  }

  def shouldRestart = false

  override def event[S <: Outcome](event: EventObject, ps: S): S = event match {
    case NewSolutionEvent(sol) =>
      logger.info(s"New solution")
      best = sol
      ps
    case _ => ps
  }

}
