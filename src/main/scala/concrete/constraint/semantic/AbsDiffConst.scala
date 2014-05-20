package concrete.constraint.semantic;

import concrete.constraint.Residues
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.UNSATException
import cspom.variable.Interval
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
      if (AbsDiffBC.unionInter(v0.dom, i0, i1 + result, i0, i1 - result)) {
        mod ::= 0
      }
      if (AbsDiffBC.unionInter(v1.dom, i1, i0 - result, i1, i0 + result)) {
        mod ::= 1
      }
    }

    mod
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

  def getEvaluation = if (intervalsOnly) 5 else {

    val d1 = v0.dom.size
    val d2 = v1.dom.size
    d1 + d2;
  }

  def simpleEvaluation = 2
}
