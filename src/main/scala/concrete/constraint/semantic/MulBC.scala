package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.Domain
import concrete.Variable
import concrete.util.Interval
import concrete.constraint.BC

/**
 * Contrainte V0 = V1 * V2.
 *
 * @author vion
 *
 */
final class MulBC(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1))
  with BC {

  def checkValues(t: Array[Int]) = t(0) == (t(1) * t(2));

  def shave() = {
    //val bounds = v0.dom.valueInterval * v1.dom.valueInterval - result.dom.valueInterval
    var mod: List[Int] = Nil
    if (result.dom.intersectVal(v0.dom.valueInterval * v1.dom.valueInterval)) {
      mod ::= 0
    }
    if (!v1.dom.valueInterval.contains(0) && v0.dom.intersectVal(result.dom.valueInterval / v1.dom.valueInterval)) {
      mod ::= 1
    }
    if (!v0.dom.valueInterval.contains(0) && v1.dom.intersectVal(result.dom.valueInterval / v0.dom.valueInterval)) {
      mod ::= 2
    }
    mod
  }

  override def toString = result + " = " + v0 + " * " + v1

  def advise(pos: Int) = 4

  val simpleEvaluation = 2
}
