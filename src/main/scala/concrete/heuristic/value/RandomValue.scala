package concrete
package heuristic
package value

import com.typesafe.scalalogging.LazyLogging

import scala.util.Random

final class RandomValue(rand: Random) extends ValueSelector with LazyLogging {

  override def toString = "random"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def select(ps: ProblemState, variable: Variable, candidates: Domain): (Outcome, Domain) = {
    val randIndex = rand.nextInt(candidates.size)
    val r = candidates.view.drop(randIndex).head
    logger.info(s"Random value is $r")
    (ps, Singleton(r))
  }

  def shouldRestart = true
}
