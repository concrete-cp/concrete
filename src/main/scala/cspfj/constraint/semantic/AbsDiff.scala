package cspfj.constraint.semantic;

import cspfj.constraint.AC3Constraint
import cspfj.problem.Domain
import cspfj.problem.Variable;
import cspfj.constraint.AbstractConstraint

final class AbsDiff(val result: Variable, val v0: Variable, val v1: Variable)
  extends AbstractConstraint(Array(result, v0, v1)) with AC3Constraint {

  def check = value(0) == math.abs(value(1) - value(2))

  override def findSupport(position: Int, index: Int) =
    position match {
      case 0 =>
        if (scope(1).domain.size < scope(2).domain.size)
          findValidTuple0(index, 1, 2);
        else
          findValidTuple0(index, 2, 1);
      case 1 => findValidTuple(index, 1, 2);
      case 2 => findValidTuple(index, 2, 1);
      case _ => throw new IndexOutOfBoundsException;
    }

  def findValidTuple0(index: Int, pos1: Int, pos2: Int): Boolean = {
    val val0 = scope(0).domain.value(index)
    if (val0 < 0) {
      return false;
    }
    val dom1 = scope(pos1).domain
    val dom2 = scope(pos2).domain

    for (i <- dom1) {

      val val1 = dom1.value(i);

      val j1 = dom2.index(val1 - val0);
      if (j1 >= 0 && dom2.present(j1)) {
        tuple(0) = index;
        tuple(pos1) = i;
        tuple(pos2) = j1;
        return true;
      }

      val j2 = dom2.index(val1 + val0);
      if (j2 >= 0 && dom2.present(j2)) {
        tuple(0) = index;
        tuple(pos1) = i;
        tuple(pos2) = j2;
        return true;
      }

    }
    return false;
  }

  def findValidTuple(index: Int, pos1: Int,
    pos2: Int): Boolean = {
    val result = this.result.domain
    val value = scope(pos1).domain.value(index);
    val dom = scope(pos2).domain;
    for (i <- dom) {
      val resIndex = result.index(math.abs(value - dom.value(i)));
      if (resIndex >= 0 && result.present(resIndex)) {
        tuple(0) = resIndex;
        tuple(pos1) = index;
        tuple(pos2) = i;
        return true;
      }
    }
    return false;
  }

  def toString = result + " = |" + v0 + " - " + v1 + "|";

  def getEvaluation = {
    val d0 = result.domain.size
    val d1 = v0.domain.size
    val d2 = v1.domain.size
    d0 * d1 + d0 * d2 + d1 * d2;
  }
}
