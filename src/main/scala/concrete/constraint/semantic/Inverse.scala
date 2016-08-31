package concrete.constraint.semantic

import concrete.Variable
import concrete.constraint.Constraint
import concrete.ProblemState
import concrete.Outcome
import concrete.constraint.StatefulConstraint
import cspom.util.BitVector
import concrete.constraint.Removals

case class Boxed[A](bv: A)

/**
 *  Constraint x(i) = j => y(j) = i
 *
 *  Note the one-sided implication.
 */
final class Inverse(x: Array[Variable], y: Array[Variable], xOffset: Int, yOffset: Int) extends Constraint(x ++ y)
    with StatefulConstraint[Boxed[BitVector]] with Removals {

  def getEvaluation(problemState: ProblemState): Int = {
    val fx = problemState(this)
    val card = fx.bv.cardinality
    card * card
  }

  def check(tuple: Array[Int]): Boolean = {
    (0 until x.length).forall { i =>
      val j = tuple(i)
      tuple(j + x.length) == i
    }
  }

  def init(ps: ProblemState): Outcome = ps.updateState(this, Boxed(BitVector.filled(x.length)))

  def revise(problemState: ProblemState, mod: BitVector): Outcome = {

    var fx = problemState(this).bv

    // Check whether variables have been assigned
    problemState.fold(mod.filter(_ < x.length).traversable) { (ps, xPos) =>
      val dom = ps.dom(x(xPos))
      if (dom.isAssigned) {
        fx -= xPos

        val xVal = dom.head

        val yPos = xVal - yOffset

        val yVal = xPos + xOffset

        ps.tryAssign(y(yPos), yVal)
      } else {
        ps
      }
    }
      .fold(fx.traversable) { (ps, xPos) =>
        ps.filterDom(x(xPos))(v => ps.dom(y(v - yOffset)).present(xPos + xOffset))
      }
      .updateState(this, Boxed(fx))

  }

  override def toString(ps: ProblemState) = {
    "inverse (" + x.map(_.toString(ps)).mkString(", ") + ") and (" + y.map(_.toString(ps)).mkString(", ") + ")"
  }

  def simpleEvaluation: Int = 2
}