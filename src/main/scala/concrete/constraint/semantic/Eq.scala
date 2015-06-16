package concrete.constraint.semantic;

import com.typesafe.scalalogging.LazyLogging

import concrete.Domain
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.BC
import concrete.constraint.BCCompanion
import concrete.constraint.Constraint
import concrete.constraint.Removals

object Eq {
  def apply(neg: Boolean, x: Variable, b: Int, y: Variable) = Seq(
    new EqAC(neg, x, b, y),
    new EqBC(neg, x, b, y))
}

/**
 * Constraint (-)x + b = y.
 *
 * @param a
 * @param x
 * @param b
 * @param y
 */
final class EqAC(val neg: Boolean, val x: Variable, val b: Int, val y: Variable, val skipIntervals: Boolean = true)
    extends Constraint(Array(x, y)) with Removals with BCCompanion {
  def this(x: Variable, y: Variable, skipIntervals: Boolean) = this(false, x, 0, y, skipIntervals)
  def this(x: Variable, y: Variable) = this(x, y, true)

  private def yValue(x: Int) =
    if (neg) -x + b else x + b

  private def xValue(y: Int) =
    if (neg) b - y else y - b

  def check(t: Array[Int]) = (if (neg) -t(0) else t(0)) + b == t(1);

  def simpleEvaluation: Int = 2

  def getEvaluation(ps: ProblemState): Int = if (skip(ps)) -1 else ps.dom(x).size + ps.dom(y).size

  override def isConsistent(ps: ProblemState) = {
    val xDom = ps.dom(x)
    val yDom = ps.dom(y)
    if (xDom.size < yDom.size) {
      xDom.exists(xv => yDom.present(yValue(xv)))
    } else {
      yDom.exists(yv => xDom.present(xValue(yv)))
    }

  }

  def revise(ps: ProblemState, modified: List[Int]) = {
    val skip = this.skip(modified)

    {
      if (skip == 0) {
        ps
      } else {
        ps.filterDom(x) { xv: Int =>
          ps.dom(y).present(yValue(xv))
        }
      }
    } andThen { ps =>
      if (skip == 1) {
        ps
      } else {
        ps.filterDom(y) { yv: Int =>
          ps.dom(x).present(xValue(yv))
        }
      }

    } andThen { ps =>
      if (ps.dom(x).size == 1) {
        ps.entail(this)
      } else {
        ps
      }
    }

  }

  override def toString(ps: ProblemState) = s"${if (neg) "-" else ""}${x.toString(ps)}${
    if (b > 0) " + " + b else if (b < 0) " - " + (-b) else ""
  } =AC= ${y.toString(ps)}"
}

final class EqBC(val neg: Boolean, val x: Variable, val b: Int, val y: Variable)
    extends Constraint(Array(x, y)) with BC with LazyLogging {

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

    (negX + b) intersects ps.span(y)
  }

  override def toString(ps: ProblemState) = s"${if (neg) "-" else ""}${x.toString(ps)}${
    if (b > 0) " + " + b else if (b < 0) " - " + (-b) else ""
  } =BC= ${y.toString(ps)}"

  def advise(ps: ProblemState, p: Int) = 3
  val simpleEvaluation = 2
}
