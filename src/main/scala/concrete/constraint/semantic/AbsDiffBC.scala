package concrete.constraint.semantic;

import concrete.constraint.Residues
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.UNSATException
import concrete.util.Interval
import concrete.UNSATObject
import concrete.constraint.BC

object AbsDiffBC {
  def unionInter(dom: Domain, i0: Interval, j0: Interval, i1: Interval, j1: Interval) = {
    val k0 = i0 intersect j0
    val k1 = i1 intersect j1

    (k0, k1) match {
      case (None, None) => throw UNSATObject
      case (None, Some(k)) => dom.intersectVal(k)
      case (Some(k), None) => dom.intersectVal(k)
      case (Some(k0), Some(k1)) => dom.intersectVal(k0 union k1) | (
        if (k0.ub < k1.lb) dom.filter(i => k0.ub >= dom.value(i) || dom.value(i) >= k1.lb)
        else if (k1.ub < k0.lb) dom.filter(i => k1.ub >= dom.value(i) || dom.value(i) >= k0.lb)
        else false)
    }
  }
}

final class AbsDiffBC(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1)) with BC {

  def checkValues(t: Array[Int]) = t(0) == math.abs(t(1) - t(2))

  def shave() = {
    val i0 = v0.dom.valueInterval
    val i1 = v1.dom.valueInterval

    val diff = i0 - i1

    var mod: List[Int] = Nil
    if (result.dom.intersectVal(diff.abs)) {
      mod ::= 0
    }
    val r = result.dom.valueInterval

    if (diff.lb >= 0) {
      if (v0.dom.intersectVal(i1 + r)) {
        mod ::= 1
      }
      if (v1.dom.intersectVal(i0 - r)) {
        mod ::= 2
      }
    } else if (diff.ub <= 0) {
      if (v0.dom.intersectVal(i1 - r)) {
        mod ::= 1
      }
      if (v1.dom.intersectVal(i0 + r)) {
        mod ::= 2
      }
    } else {
      if (AbsDiffBC.unionInter(v0.dom, i0, i1 + r, i0, i1 - r)) {
        mod ::= 1
      }
      if (AbsDiffBC.unionInter(v1.dom, i1, i0 - r, i1, i0 + r)) {
        mod ::= 2
      }
    }

    mod
  }

  override def toString = result + " =BC= |" + v0 + " - " + v1 + "|";

  def advise(pos: Int) = 5

  def simpleEvaluation = 2
}
