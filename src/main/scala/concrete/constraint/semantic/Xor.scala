package concrete.constraint.semantic;


import concrete.TRUE
import concrete.Variable
import concrete.constraint.Constraint
import concrete.Contradiction
import concrete.ProblemState
import concrete.Outcome

final class Xor(vars: Array[Variable]) extends Constraint(vars) {

  //require(reverses.size == scope.size, "reverses must cover all variables")

  private var watch1: Int = _

  private var watch2: Int = _

  override def init(ps: ProblemState): Outcome = {
    watch1 = seekWatch(ps, -1)

    if (watch1 < 0) {
      if (isOdd(ps)) {
        ps.entail(this)
      } else {
        Contradiction
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

  private def isOdd(ps: ProblemState) = {
    parity(ps) % 2 == 1
  }

  private def parity(ps: ProblemState) = {
    var c = 0
    var i = arity - 1
    while (i >= 0) {
      if (ps.dom(vars(i)) == TRUE) {
        c += 1
      }
      i -= 1
    }
    c

  }

  //if (isTrue(watch1) || isTrue(watch2)) entail()

  def advise(ps: ProblemState, p: Int) =
    if (p == watch1 || p == watch2) 1 else -1

  override def check(t: Array[Int]) = {
    t.count(_ == 1) % 2 == 1
  }

  override def toString(ps: ProblemState) =
    s"(+)(${vars.map(_.toString(ps)).mkString(", ")})"

  def revise(ps: ProblemState): Outcome = {
    val w1 = if (ps.assigned(vars(watch1))) {
      seekWatch(ps, watch2)
    } else {
      watch1
    }
    val w2 = if (ps.assigned(vars(watch2))) {
      seekWatch(ps, w1)
    } else {
      watch2
    }
    if (w1 < 0) {
      if (w2 < 0) {
        if (isOdd(ps)) {
          ps.entail(this)
        } else {
          Contradiction
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

  private def seekWatch(ps: ProblemState, excluding: Int): Int = {
    var i = arity - 1
    while (i >= 0) {
      if (i != excluding && !ps.assigned(scope(i))) {
        return i
      }
      i -= 1
    }
    -1
  }

  val simpleEvaluation = 1
}
