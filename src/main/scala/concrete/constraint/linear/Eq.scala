package concrete
package constraint
package linear

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.extension.BinaryExt
import concrete.generator.ACBC
import concrete.util.Interval

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

  def init(ps: ProblemState): Outcome = ps

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val dx = ps.dom(x)

    ps.dom(r) match {
      case BooleanDomain.UNKNOWNBoolean =>
        if (dx(y)) {
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

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
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

  var staticEvaluation: Int = _

  def this(x: Variable, y: Variable) = this(x, 0, y)

  def advise(problemState: ProblemState, event: Event, pos: Int): Int = 2

  def check(tuple: Array[Int]): Boolean = tuple(0) + b == tuple(1)

  def init(ps: ProblemState): Outcome = {
    staticEvaluation = (ps.card(x) + ps.card(y)) / BinaryExt.GAIN_OVER_GENERAL
    ps
  }

  def revise(ps: concrete.ProblemState, mod: BitVector): Outcome = {
    val oldY = ps.dom(y)
    val newX = ps.dom(x) & oldY.shift(-b)

    ps.updateDom(x, newX)
      .andThen { ps =>
        if (newX.size < oldY.size) {
          val newY = newX.shift(b)
          ps.updateDomNonEmptyNoCheck(y, newY)
        } else {
          ps
        }
      }

  }

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome =
    if (ps.dom(x).shift(b) disjoint ps.dom(y)) Contradiction(scope) else ps

  def simpleEvaluation: Int = 1

  override def toString(ps: ProblemState): String = s"${x.toString(ps)}${
    if (b > 0) s" + $b" else if (b < 0) s" − ${-b}" else ""
  } =FAC= ${y.toString(ps)}"

  override def toString: String = s"$x${
    if (b > 0) s" + $b" else if (b < 0) s" − ${-b}" else ""
  } =FAC= $y"
}

/**
  * Constraint x + y = b
  *
  * @param x
  * @param y
  * @param b
  *
  */
final class EqACNeg private[linear](
                                     val x: Variable,
                                     val y: Variable,
                                     val b: Int,
                                     val skipIntervals: Boolean = true)
  extends Constraint(Array(x, y)) with BCCompanion {

  def init(ps: ProblemState): ProblemState = ps

  def this(x: Variable, y: Variable) = this(x, y, 0, true)

  def check(t: Array[Int]): Boolean = t(0) + t(1) == b

  def simpleEvaluation: Int = 2

  def advise(ps: ProblemState, event: Event, pos: Int): Int =
    if (skip(ps)) -1 else ps.card(x) + ps.card(y)

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome = {
    val xDom = ps.dom(x)
    val yDom = ps.dom(y)
    val r = (xDom.span + yDom.span).contains(b) && (
      if (xDom.size < yDom.size) {
        xDom.exists(xv => yDom(b - xv))
      } else {
        yDom.exists(yv => xDom(b - yv))
      })

    if (r) ps else Contradiction(scope)
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val domY = ps.dom(y)
    ps.filterDom(x)(xv => domY(b - xv))
      .andThen { ps =>
        val domX = ps.dom(x)
        ps.filterDom(y)(yv => domX(b - yv))
      }

  }

  override def toString(ps: ProblemState) = s"${x.toString(ps)} + ${y.toString(ps)} =AC= $b"
}

/**
  * if (neg)
  * constraint x + y = b
  * else
  * constraint y - x = b
  */

final class EqBC(val neg: Boolean, val x: Variable, val b: Int, val y: Variable)
  extends Constraint(Array(x, y)) with BC with LazyLogging with ItvArrayFixPoint {

  val simpleEvaluation = 2
  val ops = if (neg) Array(reviseXNeg(_), reviseYNeg(_)) else Array(reviseXPos(_), reviseYPos(_))

  def init(ps: ProblemState): ProblemState = ps

  /**
    * public
    * Constraint x = y.
    *
    * @param x
    * @param y
    */
  def this(x: Variable, y: Variable) = this(false, x, 0, y)

  def check(t: Array[Int]): Boolean = (if (neg) -t(0) else t(0)) + b == t(1)

  override def revise(ps: ProblemState, mod: BitVector): Outcome =
    fixPoint(ps)

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome = {
    val xSpan = ps.span(x)

    val negX = if (neg) -xSpan else xSpan

    if ((negX + b) intersects ps.span(y)) ps else Contradiction(scope)
  }

  override def toString(ps: ProblemState) = s"${if (neg) "−" else ""}${x.toString(ps)}${
    if (b > 0) s" + $b" else if (b < 0) s" − ${-b}" else ""
  } =BC= ${y.toString(ps)}"

  def advise(ps: ProblemState, p: Int) = 3

  // x + b = y <=> x = y - b
  private def reviseXPos(doms: Array[Domain]): Option[Interval] = Some(doms(1).span - b)

  private def reviseYPos(doms: Array[Domain]): Option[Interval] = Some(doms(0).span + b)

  // -x + b = y <=> x = -y + b
  private def reviseXNeg(doms: Array[Domain]): Option[Interval] = Some(-doms(1).span + b)

  private def reviseYNeg(doms: Array[Domain]): Option[Interval] = Some(-doms(0).span + b)
}
