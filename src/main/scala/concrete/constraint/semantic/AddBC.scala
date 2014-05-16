package concrete.constraint.semantic;

import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Residues
import cspom.variable.Interval
import com.typesafe.scalalogging.slf4j.LazyLogging
import concrete.constraint.Shaver
import concrete.constraint.BC

final class AddBC(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1)) with BC with LazyLogging {

  def checkValues(t: Array[Int]) = t(0) == t(1) + t(2)

  def shave() = {
    val bounds = v0.dom.valueInterval + v1.dom.valueInterval - result.dom.valueInterval
    var mod: List[Int] = Nil
    if (reviseB(result, true, bounds)) {
      mod ::= 0
    }
    if (reviseB(v0, false, bounds)) {
      mod ::= 1
    }
    if (reviseB(v1, false, bounds)) {
      mod ::= 2
    }
    mod
  }

  private def reviseB(v: Variable, opp: Boolean, bounds: Interval) = {
    val myBounds = v.dom.valueInterval

    if (opp) {
      v.dom.intersectVal(bounds.lb + myBounds.ub, bounds.ub + myBounds.lb)
    } else {
      v.dom.intersectVal(myBounds.ub - bounds.ub, myBounds.lb - bounds.lb)
    }

  }

  override def toString = result + " = " + v0 + " + " + v1

  def advise(pos: Int) = 4

  def simpleEvaluation = 2
}
