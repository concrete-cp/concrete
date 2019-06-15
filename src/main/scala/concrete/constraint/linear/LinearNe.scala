package concrete
package constraint
package linear

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging

final class LinearNe(
                      constant: Int,
                      factors: Array[Int],
                      scope: Array[Variable]) extends Linear(constant, factors, scope, SumMode.NE) with LazyLogging {

  override def consistent(ps: ProblemState, mod: Iterable[Int]): Outcome = {
    var const = constant
    var i = arity - 1
    while (i >= 0) {
      val d = ps.dom(scope(i))
      if (d.isAssigned) {
        const -= d.singleValue * factors(i)
      } else {
        return ps
      }
      i -= 1
    }
    if (const == 0) Contradiction(scope) else ps
  }

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {

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
        Contradiction(scope)
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

  override def toString: String = toString("!=")

  override def toString(ps: ProblemState): String = toString(ps, "!=")

  def advise(ps: ProblemState, event: Event, p: Int): Int = if (event <= Assignment) arity * 2 else -1

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState): ProblemState = ps
}

