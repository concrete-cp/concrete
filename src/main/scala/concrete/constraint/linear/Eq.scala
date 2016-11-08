package concrete
package constraint
package linear

import com.typesafe.scalalogging.LazyLogging

import concrete.generator.ACBC
import concrete.constraint.extension.BinaryExt
import cspom.util.BitVector

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

final class EqCReif(val r: Variable, val x: Variable, val y: Int) extends Constraint(Array(r, x)) {

  def advise(problemState: ProblemState, event: Event, pos: Int): Int = 2
  def check(tuple: Array[Int]): Boolean = tuple(0) == (if (tuple(1) == y) 1 else 0)
  def init(ps: ProblemState): Outcome = {
    //println(toString(ps))
    ps
  }
  def revise(ps: ProblemState): Outcome = {
    val dx = ps.dom(x)

    ps.dom(r) match {
      case BooleanDomain.UNKNOWNBoolean =>
        if (dx.present(y)) {
          if (dx.isAssigned) {
            // Necessarily grounded to the same value since not disjoint
            ps.updateDomNonEmpty(r, BooleanDomain.TRUE)
          } else {
            ps
          }
        } else {
          ps.updateDomNonEmpty(r, BooleanDomain.FALSE).entail(this)

        }

      case BooleanDomain.TRUE => ps.tryAssign(x, y)

      case BooleanDomain.FALSE => ps.removeIfPresent(x, y).entail(this)

    }
  }
  def simpleEvaluation: Int = 1

  override def toString(ps: ProblemState) =
    s"${r.toString(ps)} <=> ${x.toString(ps)} = $y"
}

final class EqReif(val r: Variable, val x: Variable, val y: Variable) extends Constraint(Array(r, x, y)) {
  def advise(problemState: ProblemState, event: Event, pos: Int): Int = 3
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
        ps.updateDom(x, d)
          .andThen { ps =>
            if (d.size < dy.size) ps.updateDom(y, d) else ps
          }

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

  def advise(problemState: ProblemState, event: Event, pos: Int): Int = 2

  def check(tuple: Array[Int]): Boolean = (tuple(0) + b == tuple(1))

  def init(ps: ProblemState): Outcome = {
    staticEvaluation = (ps.card(x) + ps.card(y)) / BinaryExt.GAIN_OVER_GENERAL
    ps
  }

  def revise(ps: concrete.ProblemState): Outcome = {
    val oldY = ps.dom(y)
    val newX = ps.dom(x) & oldY.shift(-b)

    ps.updateDom(x, newX)
      .andThen { ps =>
        /*
         * ProblemState does not detect NOP after two
         * complementary offset operations, so do it by hand
         */
        if (newX.size < oldY.size) {
          val newY = newX.shift(b)
          ps.updateDomNonEmptyNoCheck(y, newY)
        } else {
          ps
        }
      }

  }

  override def isConsistent(ps: ProblemState) =
    !(ps.dom(x) disjoint ps.dom(y).shift(-b))

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

  def init(ps: ProblemState) = {
    //println(toString(ps))
    ps
  }

  def this(x: Variable, y: Variable) = this(x, y, 0, true)

  def check(t: Array[Int]) = t(0) + t(1) == b

  def simpleEvaluation: Int = 2

  def getEvaluation(ps: ProblemState): Int =
    if (skip(ps)) -1 else ps.card(x) + ps.card(y)

  override def isConsistent(ps: ProblemState, mod: BitVector) = {
    val xDom = ps.dom(x)
    val yDom = ps.dom(y)
    (xDom.span + yDom.span).contains(b) && (
      if (xDom.size < yDom.size) {
        xDom.exists(xv => yDom.present(b - xv))
      } else {
        yDom.exists(yv => xDom.present(b - yv))
      })
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

/**
 * if (neg)
 *  constraint x + y = b
 * else
 *  constraint y - x = b
 */

final class EqBC(val neg: Boolean, val x: Variable, val b: Int, val y: Variable)
    extends Constraint(Array(x, y)) with BC with LazyLogging {

  def init(ps: ProblemState) = ps

  /**
   * public
   * Constraint x = y.
   *
   * @param x
   * @param y
   */
  def this(x: Variable, y: Variable) = this(false, x, 0, y);

  def check(t: Array[Int]) = (if (neg) -t(0) else t(0)) + b == t(1);

  override def shave(ps: ProblemState) = {
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

    (negX + b) intersects ps.span(y)
  }

  override def toString(ps: ProblemState) = s"${if (neg) "-" else ""}${x.toString(ps)}${
    if (b > 0) " + " + b else if (b < 0) " - " + (-b) else ""
  } =BC= ${y.toString(ps)}"

  def advise(ps: ProblemState, p: Int) = 3
  val simpleEvaluation = 2
}
