package concrete.heuristic.value

import com.typesafe.scalalogging.LazyLogging
import concrete._

final class SmartBound() extends ValueSelector with LazyLogging {

  override def toString = "smart-bound"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def select(ps: ProblemState, variable: Variable, candidates: Domain): (Outcome, Domain) = {
    val r = if (candidates.head >= 0 && candidates.last <= 1) {
      candidates.last
    } else {
      candidates.head
    }
    logger.debug(s"Smart bound is $r")
    (ps, Singleton(r))
  }

  def shouldRestart = false

}
