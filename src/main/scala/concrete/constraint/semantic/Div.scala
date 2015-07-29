package concrete.constraint.semantic

import concrete.constraint.BC
import concrete.constraint.Constraint
import concrete.Variable
import concrete.ProblemState
import concrete.Outcome
import concrete.IntervalDomain
import concrete.util.Interval
import concrete.IntervalDomain
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import concrete.IntervalDomain
import concrete.Domain
import concrete.IntervalDomain

object Div {
  /**
   * Must use another division rules than one defined in Interval
   */
  def div(i0: Interval, i1: Interval): Interval = {
    if (i1.contains(0)) throw new ArithmeticException
    else {
      val Interval(a, b) = i0
      val Interval(c, d) = i1

      val ac = a / c
      val ad = a / d
      val bc = b / c
      val bd = b / d

      val l = math.min(ac, math.min(ad, math.min(bc, bd)))

      val u = math.max(ac, math.max(ad, math.max(bc, bd)))

      Interval(l, u)
    }
  }

  def reminder(xSpan: Interval, ySpan: Interval): Interval = {
    if (xSpan.lb >= 0) {
      Interval(0, math.max(math.abs(ySpan.lb), math.abs(ySpan.ub)) - 1)
    } else if (xSpan.ub < 0) {
      Interval(-math.max(Math.abs(ySpan.lb), math.abs(ySpan.ub)) + 1, 0);
    } else {
      Interval(math.min(math.min(ySpan.lb, -ySpan.lb), math.min(ySpan.ub, -ySpan.ub)) + 1,
        math.max(math.max(ySpan.lb, -ySpan.lb), math.max(ySpan.ub, -ySpan.ub)) - 1)
    }
  }
}
/**
 * @author vion
 * x / y = z
 */
class DivBC(x: Variable, y: Variable, z: Variable) extends Constraint(x, y, z) with BC {
  override def init(ps: ProblemState): Outcome = {
    super.init(ps).remove(y, 0)
  }

  // TODO : to finish, doesn't work
  def shave(ps: ProblemState): Outcome = {
    
    val xSpan = ps.span(x)
    val ySpan = ps.span(y)
    val zSpan = ps.span(z)
    val reminder = Div.reminder(xSpan, ySpan)

    val newY = Div.div(xSpan, zSpan - reminder)
    
    ps.shaveDom(z, Div.div(xSpan, ySpan))
      .shaveDom(y, newY)
      .andThen { ps =>
        val xBounds = zSpan * ySpan
        val r = xSpan - xBounds
        ps.shaveDom(x, xBounds + r)
      }
  }

  def advise(problemState: ProblemState, pos: Int): Int = 3
  def check(tuple: Array[Int]): Boolean = tuple(0) % tuple(1) == tuple(2)
  def simpleEvaluation: Int = 1
}

class DivAC(v0: Variable, v1: Variable, result: Variable) extends Constraint(v0, v1, result) with Residues with TupleEnumerator {
  def check(t: Array[Int]) = {
    t(0) / t(1) == t(2)

  }
}