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
/**
 * @author vion
 * x % y = z
 */
class ModBC(x: Variable, y: Variable, z: Variable) extends Constraint(x, y, z) with BC {
  override def init(ps: ProblemState): Outcome = {
    super.init(ps).remove(y, 0)
  }

  // Members declared in concrete.constraint.BC
  def shave(ps0: ProblemState): Outcome = {
    // Compute bounds for reminder 
    val xSpan = ps0.span(x)
    val ySpan = ps0.span(y)
    val reminder = Div.reminder(xSpan, ySpan)
    val result = Div.div(xSpan, ySpan)
    val zSpan = result * ySpan

    val ps1 = ps0.shaveDom(z, reminder)

    val ps2 = if (result.contains(0)) {
      ps1
    } else {
      ps1.shaveDom(y, Div.div((xSpan - reminder), result))
    }

    ps2.shaveDom(z, xSpan - zSpan)
      .andThen { ps =>
        ps.shaveDom(x, ps.dom(z).span + zSpan)
      }

  }

  // Members declared in concrete.constraint.Constraint
  def advise(problemState: ProblemState, pos: Int): Int = 3
  def check(tuple: Array[Int]): Boolean = tuple(0) % tuple(1) == tuple(2)
  def simpleEvaluation: Int = 1
}

class ModAC(v0: Variable, v1: Variable, result: Variable) extends Constraint(v0, v1, result) with Residues with TupleEnumerator {
  def check(t: Array[Int]) = {
    t(0) % t(1) == t(2)

  }
}