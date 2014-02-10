package concrete.constraint.semantic;

import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.util.Interval
import cspom.Loggable
import concrete.constraint.Shaver

final class Add(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1)) with Shaver with Residues with Loggable {

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

  def findSupport(position: Int, index: Int) = position match {
    case 0 => findValidTuple0(index);
    case 1 => findValidTupleV0(index);
    case 2 => findValidTupleV1(index);
  }

  private def findValidTuple0(index: Int) = {
    val val0 = scope(0).dom.value(index);
    val dom1 = scope(1).dom;
    val dom2 = scope(2).dom;

    dom1.indices.map { i => (i, dom2.index(val0 - dom1.value(i))) }.find {
      case (i, j) => j >= 0 && dom2.present(j)
    }.map {
      case (i, j) => Array(index, i, j)
    }
  }

  private def findValidTupleV0(index: Int) = {
    val result = scope(0).dom
    val value = scope(1).dom.value(index)
    val dom = scope(2).dom
    dom.indices.map { i => (i, result.index(value + dom.value(i))) }
      .find {
        case (i, resIndex) => resIndex >= 0 && result.present(resIndex)
      } map {
        case (i, resIndex) => Array(resIndex, index, i)
      }
  }

  private def findValidTupleV1(index: Int) = {
    val result = scope(0).dom
    val dom = scope(1).dom
    val value = scope(2).dom.value(index)

    dom.indices.map { i => (i, result.index(value + dom.value(i))) }
      .find {
        case (i, resIndex) => resIndex >= 0 && result.present(resIndex)
      } map {
        case (i, resIndex) => Array(resIndex, i, index)
      }
  }

  override def toString = result + " = " + v0 + " + " + v1

  def getEvaluation =
    if (isBound) 4
    else {
      val d0 = result.dom.size
      val d1 = v0.dom.size
      val d2 = v1.dom.size
      d0 * d1 + d0 * d2 + d1 * d2;
    }

  def simpleEvaluation = 2
}
