package cspfj.constraint.semantic;

import cspfj.problem.Domain
import cspfj.problem.Variable
import cspfj.constraint.AbstractConstraint
import cspfj.constraint.Residues
import cspfj.util.Interval

final class Add(val result: Variable, val v0: Variable, val v1: Variable)
  extends AbstractConstraint(Array(result, v0, v1)) with Residues {

  def check = value(0) == value(1) + value(2)

  override def revise() = {
    val bounds = v0.dom.valueInterval + v1.dom.valueInterval - result.dom.valueInterval
    val ch = reviseB(result, -1, bounds) | reviseB(v0, 1, bounds) | reviseB(v1, 1, bounds)

    (!isBound && super.revise()) || ch
  }

  private def reviseB(v: Variable, fact: Int, bounds: Interval) = {
    val myBounds = v.dom.valueInterval * fact
    val boundsf = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub) * -fact
    v.dom.intersectVal(boundsf)
  }

  def findSupport(position: Int, index: Int) = position match {
    case 0 => if (v0.dom.size < v1.dom.size) {
      findValidTuple0(index, 1, 2);
    } else {
      findValidTuple0(index, 2, 1);
    }
    case 1 => findValidTuple(index, 1, 2);
    case 2 => findValidTuple(index, 2, 1);
    case _ => throw new IndexOutOfBoundsException;
  }

  private def findValidTuple0(index: Int, pos1: Int, pos2: Int): Boolean = {
    val val0 = scope(0).dom.value(index);
    val dom1 = scope(pos1).dom;
    val dom2 = scope(pos2).dom;

    for (i <- dom1.indices) {
      val j = dom2.index(val0 - dom1.value(i));
      if (j >= 0 && dom2.present(j)) {
        tuple(0) = index;
        tuple(pos1) = i;
        tuple(pos2) = j;
        return true;
      }
    }
    false;
  }

  private def findValidTuple(index: Int, pos1: Int, pos2: Int): Boolean = {
    val result = scope(0).dom
    val value = scope(pos1).dom.value(index)
    val dom = scope(pos2).dom
    for (i <- dom.indices) {
      val resIndex = result.index(value + dom.value(i));
      if (resIndex >= 0 && result.present(resIndex)) {
        tuple(0) = resIndex;
        tuple(pos1) = index;
        tuple(pos2) = i;
        return true;
      }
    }
    false;
  }

  override def toString = result + " = " + v0 + " + " + v1

  def getEvaluation =
    if (isBound) 6
    else {
      val d0 = result.dom.size
      val d1 = v0.dom.size
      val d2 = v1.dom.size
      d0 * d1 + d0 * d2 + d1 * d2;
    }

  def simpleEvaluation = 2
}
