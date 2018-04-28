package concrete
package heuristic
package value

import java.util.EventObject

import com.typesafe.scalalogging.LazyLogging

final class BestValue() extends ValueSelector with LazyLogging {

  private var best: Map[Variable, Int] = Map()

  override def toString = s"best-value"

  def compute(s: MAC, ps: ProblemState): ProblemState = {
    assert(s.problem.variables.zipWithIndex.forall { case (v, i) => v.id == i })
    ps
  }

  def select(ps: ProblemState, variable: Variable, candidates: Domain): (Outcome, Domain) = {
    best.get(variable)
      .filter(candidates)
      .map { i =>
        logger.info(s"value from best solution $i")
        (ps, Singleton(i): Domain)
      }
      .getOrElse {
        logger.info(s"not present in $candidates, fallback")
        (ps, candidates)
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
