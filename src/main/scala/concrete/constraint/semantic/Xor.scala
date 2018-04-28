package concrete.constraint.semantic

;

import bitvectors.BitVector
import concrete._
import concrete.constraint.Constraint

final class Xor(vars: Array[Variable]) extends Constraint(vars) {

  //require(reverses.size == scope.size, "reverses must cover all variables")

  val simpleEvaluation = 1
  private var watch1: Int = _
  private var watch2: Int = _

  override def init(ps: ProblemState): Outcome = {
    watch1 = seekWatch(ps, -1)

    if (watch1 < 0) {
      if (isOdd(ps)) {
        ps.entail(this)
      } else {
        Contradiction(scope)
      }
    } else {
      watch2 = seekWatch(ps, watch1)
      if (watch2 < 0) {
        assignSingle(ps, watch1)
      } else {
        ps
      }
    }

  }

  def advise(ps: ProblemState, event: Event, p: Int): Int =
    if (p == watch1 || p == watch2) 1 else -1

  //if (isTrue(watch1) || isTrue(watch2)) entail()

  override def check(t: Array[Int]): Boolean = {
    t.count(_ == 1) % 2 == 1
  }

  override def toString(ps: ProblemState) =
    s"(+)(${vars.map(_.toString(ps)).mkString(", ")})"

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    logger.trace("watches are {}, {}", watch1, watch2)
    val w1 = if (ps.dom(vars(watch1)).isAssigned) {
      seekWatch(ps, watch2)
    } else {
      watch1
    }
    val w2 = if (ps.dom(vars(watch2)).isAssigned) {
      seekWatch(ps, w1)
    } else {
      watch2
    }
    if (w1 < 0) {
      if (w2 < 0) {
        if (isOdd(ps)) {
          ps.entail(this)
        } else {
          Contradiction(scope)
        }
      } else {
        assignSingle(ps, w2)
      }
    } else if (w2 < 0) {
      assignSingle(ps, w1)
    } else {
      watch1 = w1
      watch2 = w2
      ps
    }

  }

  private def assignSingle(ps: ProblemState, single: Int) = {
    if (isOdd(ps)) {
      ps.assign(vars(single), 0).entail(this)
    } else {
      ps.assign(vars(single), 1).entail(this)
    }
  }

  private def isOdd(ps: ProblemState) = {
    parity(ps) % 2 == 1
  }

  private def parity(ps: ProblemState) = {
    var c = 0
    var i = arity - 1
    while (i >= 0) {
      val d = ps.dom(vars(i))
      if (d.isAssigned && d.contains(1)) {
        c += 1
      }
      i -= 1
    }
    c

  }

  private def seekWatch(ps: ProblemState, excluding: Int): Int = {
    var i = arity - 1
    while (i >= 0) {
      if (i != excluding && !ps.dom(scope(i)).isAssigned) {
        return i
      }
      i -= 1
    }
    -1
  }
}
