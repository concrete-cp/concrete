package concrete
package constraint
package semantic

import bitvectors.BitVector
import concrete.BooleanDomain._
import concrete.cluster.Arc

case class Clause(positive: Seq[Variable], negative: Seq[Variable]) extends Arc {
  require(vars.forall(v => v.initDomain.isInstanceOf[BooleanDomain]), s"some of ${vars.map(v => s"$v ${v.initDomain}")} are not boolean")

  def size: Int = positive.size + negative.size

  def vars: Seq[Variable] = positive ++ negative

  override def toString = s"Clause(${positive.mkString(", ")}${if (positive.nonEmpty && negative.nonEmpty) ", " else ""}${negative.map("-" + _).mkString(", ")})"
}

final class ClauseConstraint(positive: Array[Variable], negative: Array[Variable]) extends Constraint(positive ++ negative: _*) {

  require(arity >= 2, "Clause constraints must have at least two variables")

  val simpleEvaluation = 1
  private val posLength = positive.length

  //require(reverses.size == scope.size, "reverses must cover all variables")

  private var watch1: Int = _

  private var watch2: Int = _

  def this(clause: Clause) = this(clause.positive.toArray, clause.negative.toArray)

  //if (isTrue(watch1) || isTrue(watch2)) entail()

  override def init(ps: ProblemState): Outcome = {

    seekEntailment(ps)
      .map { i =>
        assert {
          watch1 = i; watch1 >= 0
        }
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

  def advise(ps: ProblemState, event: Event, p: Int): Int = if (p == watch1 || p == watch2) 1 else -1

  override def check(t: Array[Int]): Boolean = {
    val (p, n) = t.view.splitAt(posLength)
    p.contains(1) || n.contains(0)
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    reviseW2(ps)
  }

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

  private def enforce(ps: ProblemState, position: Int): Outcome = {
    if (position < posLength) {
      ps.tryAssign(scope(position), 1)
    } else {
      ps.tryAssign(scope(position), 0)
    }
  }

  private def isFalse(ps: ProblemState, pos: Int): Boolean = {
    if (pos < posLength) {
      ps.dom(scope(pos)) == FALSE
    } else {
      ps.dom(scope(pos)) == TRUE
    }
  }

  private def seekWatch(ps: ProblemState, from: Int): Int = {
    var i = from
    while (i < posLength) {
      if (ps.dom(scope(i)).contains(1)) {
        return i
      }
      i += 1
    }
    while (i < arity) {
      if (ps.dom(scope(i)).contains(0)) {
        return i
      }
      i += 1
    }
    -1
  }

  private def isTrue(ps: ProblemState, pos: Int): Boolean = {
    if (pos < posLength) {
      ps.dom(scope(pos)) == TRUE
    } else {
      ps.dom(scope(pos)) == FALSE
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

  override def toString(ps: ProblemState): String =
    "\\/" + (positive.map(_.toString(ps)) ++ negative.map(v => "-" + v.toString(ps))).mkString("(", ", ", ")")

  override def controlRevision(ps: ProblemState): Boolean = {
    if (seekEntailment(ps).isEmpty) {
      require(watch1 >= 0)
      require(canBeTrue(ps, watch1) || canBeTrue(ps, watch2), s"${this.toString(ps)}: $watch1 and $watch2 are inconsistent")
      require(watch2 >= 0 || isTrue(ps, watch1))
      require(!ps.entailed.hasInactiveVar(this) || (0 until arity).exists(isTrue(ps, _)), s"entailment: ${ps.entailed.hasInactiveVar(this)}, watches $watch1, $watch2, ${this.toString(ps)}")
    }
    true
  }

  private def canBeTrue(ps: ProblemState, p: Int) = {
    if (p < posLength) {
      ps.dom(scope(p)).contains(1)
    } else {
      ps.dom(scope(p)).contains(0)
    }
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
}
