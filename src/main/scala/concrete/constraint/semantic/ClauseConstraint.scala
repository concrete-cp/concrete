package concrete
package constraint
package semantic

import BooleanDomain._

final class ClauseConstraint(positive: Array[Variable], negative: Array[Variable]) extends Constraint(positive ++ negative: _*) {

  def this(clause: Clause) = this(clause.positive.toArray, clause.negative.toArray)

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
        watch1 = seekWatch(ps, 0)

        if (watch1 < 0) {
          Contradiction(scope)
        } else {
          watch2 = seekWatch(ps, watch1 + 1)
          if (watch2 < 0) {
            enforce(ps, watch1) //.entail(this)
          } else {
            ps
          }
        }
      }
  }

  //if (isTrue(watch1) || isTrue(watch2)) entail()

  def advise(ps: ProblemState, event: Event, p: Int):Int = if (p == watch1 || p == watch2) 1 else -1

  override def check(t: Array[Int]):Boolean = {
    val (p, n) = t.view.splitAt(posLength)
    p.contains(1) || n.contains(0)
  }

  override def toString(ps: ProblemState): String =
    "\\/" + (positive.map(_.toString(ps)) ++ negative.map(v => "-" + v.toString(ps))).mkString("(", ", ", ")")

  private def reviseW2(ps: ProblemState): Outcome = {
    reversed(ps, watch2) match {
      case TRUE => ps
      case FALSE =>
        val w = seekWatch(ps, 0)
        if (w < 0) {
          Contradiction(scope)
        } else if (w == watch1) {
          // Watch1 is still valid, no need to check!
          val w2 = seekWatch(ps, w + 1)
          if (w2 < 0) {
            enforce(ps, watch1)
          } else {
            watch2 = w2
            ps
          }
        } else if (isTrue(ps, w)) {
          ps
        } else {
          watch2 = w
          // At this point, watch1 is not valid, seek for another one
          reviseW1(ps, w + 1)
        }

      case _ => reviseW1(ps, 0)
    }
  }

  private def reviseW1(ps: ProblemState, start: Int): Outcome = {
    if (isFalse(ps, watch1)) {
      var w = seekWatch(ps, start)
      if (w == watch2) {
        w = seekWatch(ps, w + 1)
      }

      if (w < 0) {
        enforce(ps, watch2) //.entail(this)
      } else {
        watch1 = w
        ps
      }
    } else {
      ps
    }
  }

  def revise(ps: ProblemState): Outcome = {
    reviseW2(ps)
  }

  override def controlRevision(ps: ProblemState): Boolean = {
    if (seekEntailment(ps).isEmpty) {
      require(watch1 >= 0)
      require(canBeTrue(ps, watch1) || canBeTrue(ps, watch2), s"${this.toString(ps)}: $watch1 and $watch2 are inconsistent")
      require(watch2 >= 0 || isTrue(ps, watch1))
      require(!ps.entailed.hasInactiveVar(this) || (0 until arity).exists(isTrue(ps, _)), s"entailment: ${ps.entailed.hasInactiveVar(this)}, watches $watch1, $watch2, ${this.toString(ps)}")
    }
    true
  }

  private def enforce(ps: ProblemState, position: Int): Outcome = {
    if (position < posLength) {
      ps.tryAssign(scope(position), 1)
    } else {
      ps.tryAssign(scope(position), 0)
    }
  }

  private def isTrue(ps: ProblemState, pos: Int): Boolean = {
    if (pos < posLength) {
      ps.dom(scope(pos)) == TRUE
    } else {
      ps.dom(scope(pos)) == FALSE
    }
  }

  private def isFalse(ps: ProblemState, pos: Int): Boolean = {
    if (pos < posLength) {
      ps.dom(scope(pos)) == FALSE
    } else {
      ps.dom(scope(pos)) == TRUE
    }
  }

  private def reversed(ps: ProblemState, pos: Int): Domain = {
    assert(pos >= 0, this.toString(ps) + " " + watch1 + " " + watch2 + " " + ps.entailed.hasInactiveVar(this))
    if (pos < posLength) {
      ps.dom(scope(pos))
    } else {
      ps.dom(scope(pos)) match {
        case TRUE => FALSE
        case FALSE => TRUE
        case e => e
      }
    }
  }

  private def canBeTrue(ps: ProblemState, p: Int) = {
    if (p < posLength) {
      ps.dom(scope(p)).present(1)
    } else {
      ps.dom(scope(p)).present(0)
    }
  }

  private def seekWatch(ps: ProblemState, from: Int): Int = {
    var i = from
    while (i < posLength) {
      if (ps.dom(scope(i)).present(1)) {
        return i
      }
      i += 1
    }
    while (i < arity) {
      if (ps.dom(scope(i)).present(0)) {
        return i
      }
      i += 1
    }
    -1
  }

  private def seekEntailment(ps: ProblemState): Option[Int] = {
    (0 until posLength)
      .find(i => ps.dom(scope(i)) == TRUE)
      .orElse {
        (posLength until arity)
          .find(i => ps.dom(scope(i)) == FALSE)
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
