package concrete.constraint.semantic

import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import concrete.util.Interval
import concrete.Domain
import concrete.ProblemState
import concrete.Outcome
import concrete.constraint.BC
import concrete.Contradiction
import concrete.constraint.BCCompanion

object Div {
  /**
   * Must use another division rules than one defined in Interval
   */
  def div(i0: Interval, i1: Interval): Option[Interval] = {

    if (i1.contains(0) && i0.lb <= 0 && i0.ub >= 0) {
      None
    } else if (i1 == Interval(0, 0) && (i0.lb > 0 || i0.ub < 0)) {
      throw new ArithmeticException
    } else if (i1.lb < 0 && i1.ub > 0 && (i0.lb > 0 || i0.ub < 0)) {
      val max = math.max(Math.abs(i0.lb), Math.abs(i0.ub))
      Some(Interval(-max, max))
    } else if (i1.lb == 0 && i1.ub != 0 && (i0.lb > 0 || i0.ub < 0)) {
      div(i0, Interval(1, i1.ub))
    } else if (i1.lb != 0 && i1.ub == 0 && (i0.lb > 0 || i0.ub < 0)) {
      div(i0, Interval(i1.lb, -1))
    } else {
      val Interval(a, b) = i0
      val Interval(c, d) = i1

      val ac = a / c
      val ad = a / d
      val bc = b / c
      val bd = b / d

      val l = math.min(ac, math.min(ad, math.min(bc, bd)))

      val u = math.max(ac, math.max(ad, math.max(bc, bd)))

      Some(Interval(l, u))
    }
  }

  def reminder(xSpan: Interval, ySpan: Interval): Interval = {
    if (xSpan.lb >= 0) {
      Interval(0, math.max(math.abs(ySpan.lb), math.abs(ySpan.ub)) - 1)
    } else if (xSpan.ub < 0) {
      Interval(-math.max(Math.abs(ySpan.lb), math.abs(ySpan.ub)) + 1, 0)
    } else {
      Interval(math.min(math.min(ySpan.lb, -ySpan.lb), math.min(ySpan.ub, -ySpan.ub)) + 1,
        math.max(math.max(ySpan.lb, -ySpan.lb), math.max(ySpan.ub, -ySpan.ub)) - 1)
    }
  }
}
///**
// * @author vion
// * x / y = z
// * x = z * y + x % y
// */
class DivBC(x: Variable, y: Variable, z: Variable) extends Constraint(x, y, z) with BC {
  def check(t: Array[Int]) = {
    t(0) / t(1) == t(2)
  }

  def advise(ps: ProblemState, pos: Int) = 3

  def init(ps: ProblemState): Outcome = ps.remove(y, 0)

  override def shave(ps: ProblemState) = {

    val x = ps.span(this.x)
    val y = ps.span(this.y)
    val z = ps.span(this.z)

    val reminder = Div.reminder(x, y)

    Div.div(x, y).map(ps.shaveDom(this.z, _))
      .getOrElse(ps)
      .andThen { ps =>
        Div.div(x - reminder, z).map(ps.shaveDom(this.y, _))
          .getOrElse(ps)
      }
      .andThen { ps =>

        val xBounds = z * y

        ((x - xBounds) intersect reminder)
          .map(r => ps.shaveDom(this.x, xBounds + r))
          .getOrElse(Contradiction)
        //
        //        if (reminderMin > rMin) rMin = reminderMin
        //        if (reminderMax < rMax) rMax = reminderMax
        //        if (rMin > rMax)
        //          Contradiction
        //        else
        //          ps.shaveDom(this.x, xBounds.lb + rMin, xBounds.ub + rMax)
      }
  }

  def simpleEvaluation: Int = 1
}

class DivAC(v0: Variable, v1: Variable, result: Variable) extends Constraint(v0, v1, result) with Residues with TupleEnumerator
    with BCCompanion {
  def skipIntervals = true
  def check(t: Array[Int]) = {
    t(0) / t(1) == t(2)

  }

  override def findSupport(doms: Array[Domain], position: Int, value: Int): Option[Array[Int]] = {
    position match {
      case 0 => doms(1).find(y => doms(2).present(value / y)).map(y => Array(value, y, value / y))
      case 1 => doms(0).find(x => doms(2).present(x / value)).map(x => Array(x, value, x / value))
      case 2 => super[TupleEnumerator].findSupport(doms, position, value)
    }
  }

}