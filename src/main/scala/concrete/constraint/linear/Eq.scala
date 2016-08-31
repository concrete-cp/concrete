package concrete.constraint.linear;

import com.typesafe.scalalogging.LazyLogging
import concrete.Contradiction
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.BC
import concrete.constraint.BCCompanion
import concrete.constraint.Constraint
import concrete.constraint.Removals
import concrete.Outcome
import concrete.generator.ACBC
import concrete.constraint.extension.BinaryExt
import cspom.util.BitVector
import concrete.BooleanDomain.UNKNOWNBoolean
import concrete.BooleanDomain

object Eq {
  def apply(neg: Boolean, x: Variable, b: Int, y: Variable): ACBC =
    if (neg) {
      ACBC
        .withAC(new EqACNeg(x, y, b))
        .withBC(new EqBC(neg, x, b, y))
    } else {
      ACBC
        .withAC(new EqACFast(x, b, y))
    }
}

final class EqReif(val r: Variable, val x: Variable, val y: Variable) extends Constraint(Array(r, x, y)) {
  def advise(problemState: ProblemState, pos: Int): Int = 3
  def check(tuple: Array[Int]): Boolean = tuple(0) == (if (tuple(1) == tuple(2)) 1 else 0)
  def init(ps: ProblemState): Outcome = ps
  def revise(ps: ProblemState): Outcome = {
    val dx = ps.dom(x)
    val dy = ps.dom(y)
    ps.dom(r) match {
      case BooleanDomain.UNKNOWNBoolean =>
        if (dx disjoint dy) {
          ps.updateDomNonEmpty(r, BooleanDomain.FALSE).entail(this)
        } else if (dx.isAssigned && dy.isAssigned) {
          // Necessarily grounded to the same value since not disjoint
          ps.updateDomNonEmpty(r, BooleanDomain.TRUE).entail(this)
        } else {
          ps
        }

      case BooleanDomain.TRUE =>
        val d = dx & dy
        ps.updateDom(x, d).updateDom(y, d)

      case BooleanDomain.FALSE =>
        (if (dx.isAssigned) ps.removeIfPresent(y, dx.head) else ps)
          .andThen { ps =>
            if (dy.isAssigned) ps.removeIfPresent(x, dy.head) else ps
          }
          .entailIf(this, ps =>
            ps.dom(x) disjoint ps.dom(y))

    }
  }
  def simpleEvaluation: Int = 1
}

/**
 * Constraint x + b = y
 */
final class EqACFast(val x: Variable, val b: Int, val y: Variable)
    extends Constraint(Array(x, y)) {

  def this(x: Variable, y: Variable) = this(x, 0, y)

  var staticEvaluation: Int = _

  def advise(problemState: ProblemState, pos: Int): Int = 2

  def check(tuple: Array[Int]): Boolean = (tuple(0) + b == tuple(1))

  def init(ps: ProblemState): Outcome = {
    staticEvaluation = (ps.card(x) + ps.card(y)) / BinaryExt.GAIN_OVER_GENERAL
    ps
  }

  def revise(ps: concrete.ProblemState): Outcome = {
    val oldX = ps.dom(x)
    val oldY = ps.dom(y)
    val newX = oldX & oldY.shift(-b)

    ps.updateDom(x, newX)
      .andThen { ps =>
        val newY = newX.shift(b)
        /*
         * ProblemState does not detect NOP after two
         * complementary offset operations, so do it by hand
         */
        if (newY.size < oldY.size) {
          ps.updateDomNonEmptyNoCheck(y, newY)
        } else {
          ps
        }
      }

  }

  override def isConsistent(ps: ProblemState): Outcome = {
    if (ps.dom(x) disjoint ps.dom(y).shift(-b)) {
      Contradiction
    } else {
      ps
    }
  }

  def simpleEvaluation: Int = 1
  override def toString(ps: ProblemState) = s"${x.toString(ps)}${
    if (b > 0) " + " + b else if (b < 0) " - " + (-b) else ""
  } =FAC= ${y.toString(ps)}"
}

/**
 * Constraint x + y = b
 *
 * @param a
 * @param x
 * @param b
 * @param y
 */
final class EqACNeg private[linear] (
  val x: Variable,
  val y: Variable,
  val b: Int,
  val skipIntervals: Boolean = true)
    extends Constraint(Array(x, y)) with Removals with BCCompanion {

  def init(ps: ProblemState) = ps

  def this(x: Variable, y: Variable) = this(x, y, 0, true)

  def check(t: Array[Int]) = t(0) + t(1) == b

  def simpleEvaluation: Int = 2

  def getEvaluation(ps: ProblemState): Int =
    if (skip(ps)) -1 else ps.card(x) + ps.card(y)

  override def isConsistent(ps: ProblemState) = {
    val xDom = ps.dom(x)
    val yDom = ps.dom(y)
    val r = if (xDom.size < yDom.size) {
      xDom.exists(xv => yDom.present(b - xv))
    } else {
      yDom.exists(yv => xDom.present(b - yv))
    }

    if (r) ps else Contradiction

  }

  def revise(ps: ProblemState, modified: BitVector) = {
    val skip = this.skip(modified)

    {
      if (skip == 0) {
        ps
      } else {
        val domY = ps.dom(y)
        ps.filterDom(x)(xv => domY.present(b - xv))
      }
    }
      .andThen { ps =>
        if (skip == 1) {
          ps
        } else {
          val domX = ps.dom(x)
          ps.filterDom(y)(yv => domX.present(b - yv))
        }

      }

  }

  override def toString(ps: ProblemState) = s"${x.toString(ps)} + ${y.toString(ps)} =AC= $b"
}

final class EqBC(val neg: Boolean, val x: Variable, val b: Int, val y: Variable)
    extends Constraint(Array(x, y)) with BC with LazyLogging {

  def init(ps: ProblemState) = ps

  //  val corresponding = Array(
  //    x.dom.allValues map { v => y.dom.index(a * v + b) },
  //    y.dom.allValues map { v =>
  //      val r = v - b
  //      if (r % a == 0) x.dom.index(r / a) else -1
  //    })

  /**
   * public
   * Constraint x = y.
   *
   * @param x
   * @param y
   */
  def this(x: Variable, y: Variable) = this(false, x, 0, y);

  def check(t: Array[Int]) = (if (neg) -t(0) else t(0)) + b == t(1);

  def shave(ps: ProblemState) = {
    if (neg) {
      // -x + b = y <=> x = -y + b 
      ps.shaveDom(x, -ps.span(y) + b)
        .shaveDom(y, -ps.span(x) + b)

    } else {
      // x + b = y <=> x = y - b
      ps.shaveDom(x, ps.span(y) - b)
        .shaveDom(y, ps.span(x) + b)

    }

  }

  override def isConsistent(ps: ProblemState) = {
    val xSpan = ps.span(x)

    val negX = if (neg) -xSpan else xSpan

    if ((negX + b) intersects ps.span(y)) ps else Contradiction
  }

  override def toString(ps: ProblemState) = s"${if (neg) "-" else ""}${x.toString(ps)}${
    if (b > 0) " + " + b else if (b < 0) " - " + (-b) else ""
  } =BC= ${y.toString(ps)}"

  def advise(ps: ProblemState, p: Int) = 3
  val simpleEvaluation = 2
}
