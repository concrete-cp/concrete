package cspfj.constraint.semantic;

import cspfj.problem.Domain
import cspfj.problem.Variable
import cspfj.constraint.AbstractConstraint
import cspfj.constraint.Residues
import cspfj.util.Interval
import cspfj.util.Loggable

final class Add(val result: Variable, val v0: Variable, val v1: Variable)
  extends AbstractConstraint(Array(result, v0, v1)) with Residues with Loggable {

  def check = value(0) == value(1) + value(2)

  private def shave() = {
    val bounds = v0.dom.valueInterval + v1.dom.valueInterval - result.dom.valueInterval
    reviseB(result, true, bounds) | reviseB(v0, false, bounds) | reviseB(v1, false, bounds)
  }

  override def revise() = {

    var ch = false
    while (shave()) {
      ch = true
    }

    assert(!isBound || boundConsistent)

    if (isBound) entailCheck(ch)
    else ch |= super.revise()

    ch

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
    case 0 => if (v0.dom.size < v1.dom.size) {
      findValidTuple0(index, 1, 2);
    } else {
      findValidTuple0(index, 2, 1);
    }
    case 1 => findValidTuple(index, 1, 2);
    case 2 => findValidTuple(index, 2, 1);
  }

  private def findValidTuple0(index: Int, pos1: Int, pos2: Int): Boolean = {
    val val0 = scope(0).dom.value(index);
    val dom1 = scope(pos1).dom;
    val dom2 = scope(pos2).dom;

    dom1.indices.exists { i =>
      val j = dom2.index(val0 - dom1.value(i));
      if (j >= 0 && dom2.present(j)) {
        tuple(0) = index;
        tuple(pos1) = i;
        tuple(pos2) = j;
        true;
      } else false
    }
  }

  private def findValidTuple(index: Int, pos1: Int, pos2: Int): Boolean = {
    val result = scope(0).dom
    val value = scope(pos1).dom.value(index)
    val dom = scope(pos2).dom
    dom.indices.exists { i =>
      val resIndex = result.index(value + dom.value(i));
      if (resIndex >= 0 && result.present(resIndex)) {
        tuple(0) = resIndex;
        tuple(pos1) = index;
        tuple(pos2) = i;
        true;
      } else false
    }
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
