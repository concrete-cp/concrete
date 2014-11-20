package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.Domain
import concrete.Variable
import concrete.util.Interval
import concrete.constraint.BC
import concrete.constraint.StatelessBC
import concrete.Revised

/**
 * Contrainte V0 = V1 * V2.
 *
 * @author vion
 *
 */
final class MulBC(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1))
  with StatelessBC {

  def check(t: Array[Int]) = t(0) == (t(1) * t(2));

  def shave(dom: IndexedSeq[Domain]) = {

    val rspan = dom(0).span
    val v0span = dom(1).span
    val v1span = dom(2).span

    val result = dom(0) & (v0span * v1span)

    val v0 = if (v1span.contains(0)) {
      dom(1)
    } else {
      dom(1) & (rspan / v1span)
    }

    val v1 = if (v0span.contains(0)) {
      dom(2)
    } else {
      dom(2) & (rspan / v0span)
    }

    Revised(Vector(result, v0, v1))
  }

  override def toString = result + " = " + v0 + " * " + v1

  def advise(dom: IndexedSeq[Domain], pos: Int) = 4

  val simpleEvaluation = 2
}
