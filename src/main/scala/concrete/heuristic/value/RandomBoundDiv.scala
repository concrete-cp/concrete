package concrete.heuristic.value

import com.typesafe.scalalogging.LazyLogging
import concrete._

import scala.util.Random

final class RandomBoundDiv(rand: Random) extends ValueSelector with LazyLogging {

  override def toString = "random-bound-div"

  private val factor = .1d

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  val randSelector = new RandomBound(rand)

  override def select(ps: ProblemState, variable: Variable, candidates: Domain): (Outcome, Domain) = {
    if (rand.nextDouble() < factor) {
      logger.info("Random diversification")
      randSelector.select(ps, variable, candidates)
    } else {
      logger.info(s"no random div, continue")
      (ps, candidates)
    }


  }

  def shouldRestart = true
}
