package concrete
package heuristic
package variable

import java.util.EventObject

import com.typesafe.scalalogging.LazyLogging

import scala.util.Random

class RandomDiv(val pool: Seq[Variable], factor: Double, rand: Random)
  extends VariableHeuristic with LazyLogging {

  private val poolSet: Set[Variable] = pool.toSet

  def select(state: ProblemState, i: Seq[Variable]): Seq[Variable] = {
    val p = i.toStream.filter(poolSet)
    if (rand.nextDouble() < factor) {
      logger.info(s"Performed random div")
      val r = rand.nextInt(p.size)
      p.slice(r, r + 1)
    } else {
      p
    }
  }

  override def compute(s: MAC, state: ProblemState): ProblemState = state

  override def shouldRestart: Boolean = factor > 0

  override def toString: String = s"Random div $factor"

  def event[S <: Outcome](e: EventObject, ps: S): S = ps
}
