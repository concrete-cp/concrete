package concrete.constraint.semantic;

import concrete.constraint.Residues
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.UNSATException
import concrete.util.Interval
import concrete.constraint.Shaver
import concrete.UNSATObject

final class AbsDiffConst(val result: Int, val v0: Variable, val v1: Variable)
  extends Constraint(Array(v0, v1)) with Shaver with Residues {

  def checkValues(t: Array[Int]) = result == math.abs(t(0) - t(1))

  def shave() = {
    val i0 = v0.dom.valueInterval
    val i1 = v1.dom.valueInterval

    val diff = i0 - i1

    if (!diff.abs.contains(result)) throw UNSATObject

    var mod: List[Int] = Nil

    if (diff.lb >= 0) {
      if (v0.dom.intersectVal(i1 + result)) {
        mod ::= 0
      }
      if (v1.dom.intersectVal(i0 - result)) {
        mod ::= 1
      }
    } else if (diff.ub <= 0) {
      if (v0.dom.intersectVal(i1 - result)) {
        mod ::= 0
      }
      if (v1.dom.intersectVal(i0 + result)) {
        mod ::= 1
      }
    } else {
      if (unionInter(v0.dom, i0, i1 + result, i0, i1 - result)) {
        mod ::= 0
      }
      if (unionInter(v1.dom, i1, i0 - result, i1, i0 + result)) {
        mod ::= 1
      }
    }

    mod
  }

  private def unionInter(dom: Domain, i0: Interval, j0: Interval, i1: Interval, j1: Interval) =
    (i0 intersect j0, i1 intersect j1) match {
      case (Some(k0), Some(k1)) => dom.intersectVal(k0 union k1) | (
        if (k0.ub < k1.lb) dom.filter(i => k0.ub >= dom.value(i) || dom.value(i) >= k1.lb)
        else if (k1.ub < k0.lb) dom.filter(i => k1.ub >= dom.value(i) || dom.value(i) >= k0.lb)
        else false)

      case (Some(k0), None) => dom.intersectVal(k0)
      case (None, Some(k1)) => dom.intersectVal(k1)
      case _ => throw UNSATObject
    }

  override def findSupport(position: Int, index: Int) = {
    val other = scope(1 - position).dom

    val value = scope(position).dom.value(index);

    Seq(value + result, value - result).iterator.map(other.index).find(
      i => i >= 0 && other.present(i)).map { i =>

        val tuple = new Array[Int](2)
        tuple(position) = index
        tuple(1 - position) = i
        tuple
      }

  }

  override def toString = result + " = |" + v0 + " - " + v1 + "|";

  def getEvaluation = if (isBound) 5 else {

    val d1 = v0.dom.size
    val d2 = v1.dom.size
    d1 + d2;
  }

  def simpleEvaluation = 2
}
