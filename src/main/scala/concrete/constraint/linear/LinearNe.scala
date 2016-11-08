package concrete
package constraint
package linear

import com.typesafe.scalalogging.LazyLogging

import cspom.util.BitVector
import concrete.util.Interval

final class LinearNe2(
    constant: Int,
    factors: Array[Int],
    scope: Array[Variable]) extends Linear(constant, factors, scope, SumNE) with LazyLogging {

  override def isConsistent(ps: ProblemState): Boolean = {
    var const = constant
    var i = arity - 1
    while (i >= 0) {
      val d = ps.dom(scope(i))
      if (d.isAssigned) {
        const -= d.singleValue * factors(i)
      } else {
        return true
      }
      i -= 1
    }
    const != 0
  }

  override def revise(ps: ProblemState): Outcome = {

    var const = constant
    var free = -1
    var i = arity - 1
    while (i >= 0) {
      val d = ps.dom(scope(i))
      if (d.isAssigned) {
        const -= d.singleValue * factors(i)
      } else if (free < 0) {
        free = i
      } else {
        return ps
      }
      i -= 1
    }

    if (free < 0) {
      if (const == 0) {
        Contradiction
      } else {
        ps
      }
    } else if (const % factors(free) == 0) {

      ps.removeIfPresent(scope(free), const / factors(free))
        .entail(this, free)

    } else {
      ps.entail(this, free)
    }

  }

  override def toString() = toString("!=")

  override def toString(ps: ProblemState) = toString(ps, "!=")

  def advise(ps: ProblemState, event: Event, p: Int) = if (event <= Assignment) arity * 2 else -1

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState) = ps
}

