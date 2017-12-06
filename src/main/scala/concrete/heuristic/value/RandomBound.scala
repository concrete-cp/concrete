package concrete.heuristic.value

import com.typesafe.scalalogging.LazyLogging
import concrete.{Domain, MAC, ProblemState, Variable}

import scala.util.Random

final class RandomBound(rand: Random) extends ValueHeuristic with LazyLogging {

  override def toString = "random-bound"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def selectIndex(variable: Variable, dom: Domain): Int = {
    val r = if (rand.nextBoolean()) {
      dom.head
    } else {
      dom.last
    }
    logger.info(s"Random bound is $r")
    r
  }

  def shouldRestart = true
}
