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

final class ClauseConstraint(clause: Clause) extends Constraint(clause.vars: _*) {

  private val positive = clause.positive.toArray
  private val negative = clause.negative.toArray

  private val posLength = positive.length

  //require(reverses.size == scope.size, "reverses must cover all variables")

  private var watch1: Int = _

  private var watch2: Int = _

  override def init(ps: ProblemState): Outcome = {
    seekEntailment(ps)
      .map { i =>
        assert { watch1 = i; watch1 >= 0 }
        ps.entail(this)
      }
      .getOrElse {
        watch1 = seekWatch(ps, -1)

        if (watch1 < 0) {
          Contradiction
        } else {
          watch2 = seekWatch(ps, watch1)
          if (watch2 < 0) {
            enforce(ps, watch1).entail(this)
          } else {
            ps
          }
        }
      }
  }

  private val ids = scope.map(_.id)

  //if (isTrue(watch1) || isTrue(watch2)) entail()

  def advise(ps: ProblemState, p: Int) = if (p == watch1 || p == watch2) 1 else -1

  override def check(t: Array[Int]) = {
    val (p, n) = t.view.splitAt(posLength)
    p.contains(1) || n.contains(0)
  }

  override def toString(ps: ProblemState) =
    "\\/" + (positive.map(_.toString(ps)) ++ negative.map(v => "-" + v.toString(ps))).mkString("(", ", ", ")")

  def reviseW2(ps: ProblemState): Outcome = {
    reversed(ps, watch2) match {
      case TRUE => ps.entail(this)
      case FALSE =>
        val w = seekWatch(ps, watch1)
        if (w < 0) {
          enforce(ps, watch1).entail(this)
        } else {
          watch2 = w
          if (isTrue(ps, w)) {
            ps.entail(this)
          } else {
            reviseW1From(ps, w + 1)
          }
        }
      case _ => reviseW1(ps)
    }
  }

  def reviseW1(ps: ProblemState): Outcome = {
    reversed(ps, watch1) match {
      case TRUE => ps.entail(this)
      case FALSE =>
        val w = seekWatch(ps, watch2)
        if (w < 0) {
          enforce(ps, watch2).entail(this)
        } else {
          watch1 = w
          if (isTrue(ps, w)) {
            ps.entail(this)
          } else {
            ps
          }
        }
      case _ => ps
    }
  }

  def reviseW1From(ps: ProblemState, startAt: Int): Outcome = {
    reversed(ps, watch1) match {
      case TRUE => ps.entail(this)
      case FALSE =>
        val w = seekWatchFrom(ps, startAt)
        if (w < 0) {
          enforce(ps, watch2).entail(this)
        } else {
          watch1 = w
          if (isTrue(ps, w)) {
            ps.entail(this)
          } else {
            ps
          }
        }
      case _ => ps
    }
  }

  def revise(ps: ProblemState): Outcome = {
    reviseW2(ps)
  }

  override def controlRevision(ps: ProblemState): Boolean = {

    require(watch1 >= 0)
    require(canBeTrue(ps, watch1) || canBeTrue(ps, watch2), s"${this.toString(ps)}: $watch1 and $watch2 are inconsistent")
    require(watch2 >= 0 || isTrue(ps, watch1))
    require(!ps.isEntailed(this) || (0 until arity).exists(isTrue(ps, _)), s"entailment: ${ps.isEntailed(this)}, watches $watch1, $watch2, ${this.toString(ps)}")
    true
  }

  private def enforce(ps: ProblemState, position: Int): Outcome = {
    if (position < posLength) {
      ps.assign(ids(position), 1)
    } else {
      ps.assign(ids(position), 0)
    }
  }

  private def isTrue(ps: ProblemState, pos: Int): Boolean = {
    if (pos < posLength) {
      ps.dom(ids(pos)) == TRUE
    } else {
      ps.dom(ids(pos)) == FALSE
    }
  }

  private def isFalse(ps: ProblemState, pos: Int): Boolean = {
    if (pos < posLength) {
      ps.dom(ids(pos)) == FALSE
    } else {
      ps.dom(ids(pos)) == TRUE
    }
  }

  private def reversed(ps: ProblemState, pos: Int): BooleanDomain = {
    if (pos < posLength) {
      ps.boolDom(ids(pos))
    } else {
      ps.boolDom(ids(pos)) match {
        case TRUE  => FALSE
        case FALSE => TRUE
        case e     => e
      }
    }
  }

  private def canBeTrue(ps: ProblemState, p: Int) = {
    ps.boolDom(ids(p)).canBe(p < posLength)
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

  private def seekWatchFrom(ps: ProblemState, startAt: Int): Int = {
    var i = startAt
    while (i < posLength) {
      if (ps.boolDom(ids(i)).canBe(true)) {
        return i
      }
      i += 1
    }
    while (i < arity) {
      if (ps.boolDom(ids(i)).canBe(false)) {
        return i
      }
      i += 1
    }
    -1
  }

  private def seekEntailment(ps: ProblemState): Option[Int] = {
    (0 until posLength)
      .find(i => ps.dom(ids(i)) == TRUE)
      .orElse {
        (posLength until arity)
          .find(i => ps.dom(ids(i)) == FALSE)
      }
    //    var i = 0
    //    while (i < posLength) {
    //      if (ps.dom(ids(i)) == TRUE) {
    //        return Some(i)
    //      }
    //      i += 1
    //    }
    //    while (i < arity) {
    //      if (ps.dom(ids(i)) == FALSE) {
    //        return Some(i)
    //      }
    //      i += 1
    //    }
    //    None
  }

  val simpleEvaluation = 1
}
