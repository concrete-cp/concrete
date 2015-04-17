package concrete.constraint.semantic;

import scala.annotation.tailrec
import concrete.BooleanDomain
import concrete.Domain
import concrete.FALSE
import concrete.TRUE
import concrete.Variable
import concrete.constraint.Constraint
import concrete.Contradiction
import concrete.ProblemState
import concrete.Outcome
import concrete.constraint.AdviseCount
import concrete.constraint.AdviseCounts

final class Clause(positive: Array[Variable], negative: Array[Variable]) extends Constraint(positive ++ negative)
  with AdviseCounts {

  val posLength = positive.length

  //require(reverses.size == scope.size, "reverses must cover all variables")

  private var watch1: Int = _

  private var watch2: Int = _

  private var modW1: Int = -1
  private var modW2: Int = -1

  override def init(ps: ProblemState): Outcome = {
    watch1 = seekWatch(ps, -1)

    if (watch1 < 0) {
      Contradiction
    } else if (isTrue(ps.dom(watch1), watch1)) {
      ps.entail(this)
    } else {

      watch2 = seekWatch(ps, watch1)
      if (watch2 < 0) {
        enforce(ps, watch1).entail(this)
      } else if (isTrue(ps.dom(watch2), watch2)) {
        ps.entail(this)
      } else {
        ps
      }
    }

  }

  private val ids = scope.map(_.id)

  //if (isTrue(watch1) || isTrue(watch2)) entail()

  def advise(ps: ProblemState, p: Int) =
    if (p == watch1) {
      modW1 = adviseCount
      1
    } else if (p == watch2) {
      modW2 = adviseCount
      1
    } else {
      -1
    }

  override def check(t: Array[Int]) = {
    val (p, n) = t.view.splitAt(positive.length)
    p.contains(1) || n.contains(0)
  }

  override def toString(ps: ProblemState) =
    "\\/" + (positive.map(_.toString(ps)) ++ negative.map(v => "-" + v.toString(ps))).mkString("(", ", ", ")")

  def reviseW2(ps: ProblemState): Outcome = {
    if (modW2 == adviseCount) {
      modW2 = -1
      val domW2 = ps.dom(watch2)
      if (isTrue(domW2, watch2)) {
        ps.entail(this)
      } else if (isFalse(domW2, watch2)) {
        val w = seekWatch(ps, watch1)
        if (w < 0) {
          enforce(ps, watch1).entail(this)
        } else if (isTrue(ps.dom(w), w)) {
          ps.entail(this)
        } else {
          watch2 = w
          reviseW1(ps)
        }

      } else {
        reviseW1(ps)
      }
    } else {
      reviseW1(ps)
    }
  }

  def reviseW1(ps: ProblemState): Outcome = {
    if (modW1 == adviseCount) {
      modW1 = -1
      val domW1 = ps.dom(watch1)
      if (isTrue(domW1, watch1)) {
        ps.entail(this)
      } else if (isFalse(domW1, watch1)) {
        val w = seekWatch(ps, watch2)
        if (w < 0) {
          enforce(ps, watch2).entail(this)
        } else if (isTrue(ps.dom(w), w)) {
          ps.entail(this)
        } else {
          watch1 = w
          ps
        }

      } else {
        ps
      }
    } else {
      ps
    }
  }

  def revise(ps: ProblemState): Outcome = {
    reviseW2(ps)

  }

  private def enforce(ps: ProblemState, position: Int): Outcome = {
    if (position < posLength) {
      ps.assign(ids(position), 1)
    } else {
      ps.assign(ids(position), 0)
    }
  }

  private def isTrue(dom: Domain, pos: Int): Boolean = {
    if (pos < posLength) {
      dom == TRUE
    } else {
      dom == FALSE
    }
  }

  private def isFalse(dom: Domain, pos: Int): Boolean = {
    if (pos < posLength) {
      dom == FALSE
    } else {
      dom == TRUE
    }
  }

  private def seekWatch(ps: ProblemState, excluding: Int): Int = {
    var i = 0
    while (i < posLength) {
      if (i != excluding && ps.boolDom(ids(i)).canBe(true)) {
        return i
      }
      i += 1
    }
    while (i < arity) {
      if (i != excluding && ps.boolDom(ids(i)).canBe(false)) {
        return i
      }
      i += 1
    }
    -1
  }

  val simpleEvaluation = 1
}
