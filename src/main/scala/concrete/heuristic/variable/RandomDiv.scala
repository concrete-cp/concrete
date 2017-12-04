package concrete
package heuristic
package variable

import java.util.EventObject

import com.typesafe.scalalogging.LazyLogging

import scala.util.Random

class RandomDiv(actual: VariableHeuristic, factor: Double, rand: Random)
  extends VariableHeuristic with LazyLogging {

  private val random = new RandomVar(pool, rand)

  def pool: Seq[Variable] = actual.pool

  def select(state: ProblemState, i: Seq[Variable]): Option[Variable] = {
    if (rand.nextDouble() < factor) {
      logger.info(s"Performed random div")
      random.select(state, i)
    } else {
      actual.select(state, i)
    }
  }

  override def compute(s: MAC, state: ProblemState): ProblemState = {
    random.compute(s, actual.compute(s, state))
  }

  override def shouldRestart: Boolean = factor > 0 || actual.shouldRestart

  override def toString: String = s"Random div $factor for $actual"

  def event[S <: Outcome](e: EventObject, ps: S): S = actual.event(e, ps)
}
