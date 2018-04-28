package concrete.heuristic.value

import com.typesafe.scalalogging.LazyLogging
import concrete._

import scala.util.Random

final class RandomBound(rand: Random) extends ValueSelector with LazyLogging {

  override def toString = "random-bound"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def select(ps: ProblemState, variable: Variable, candidates: Domain): (Outcome, Domain) = {
    val r = if (rand.nextBoolean()) {
      Singleton(candidates.head)
    } else {
      Singleton(candidates.last)
    }
    logger.info(s"Random bound is $r")
    (ps, r)
  }

  def shouldRestart = true
}
