package concrete.constraint.semantic

import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.BC
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import concrete.Domain

/**
 * @author vion
 * x % y = z
 */
class ModBC(x: Variable, y: Variable, z: Variable) extends Constraint(x, y, z) with BC {
  override def init(ps: ProblemState): Outcome = ps.remove(y, 0)

  // Members declared in concrete.constraint.BC
  override def shave(ps0: ProblemState): Outcome = {
    // Compute bounds for reminder 
    val xSpan = ps0.span(x)
    val ySpan = ps0.span(y)
    val reminder = Div.reminder(xSpan, ySpan)

    ps0.shaveDom(z, reminder)
      .andThen { ps1 =>

        val result = Div.div(xSpan, ySpan).get
        val zSpan = result * ySpan

        Div.div((xSpan - reminder), result)
          .map(ps1.shaveDom(y, _))
          .getOrElse(ps1)
          .shaveDom(z, xSpan - zSpan)
          .andThen { ps =>
            ps.shaveDom(x, ps.dom(z).span + zSpan)
          }
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

  override def findSupport(doms: Array[Domain], position: Int, value: Int): Option[Array[Int]] =
    super[TupleEnumerator].findSupport(doms, position, value)
}