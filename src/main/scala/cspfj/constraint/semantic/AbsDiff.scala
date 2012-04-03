package cspfj.constraint.semantic;

import cspfj.constraint.Residues
import cspfj.problem.Domain
import cspfj.problem.Variable
import cspfj.constraint.Constraint
import cspfj.UNSATException
import cspfj.util.Interval

final class AbsDiff(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1)) with Residues {

  def checkValues(t: Array[Int]) = t(0) == math.abs(t(1) - t(2))

  def shave() = {
    val i0 = v0.dom.valueInterval
    val i1 = v1.dom.valueInterval

    val diff = i0 - i1

    var ch = result.dom.intersectVal(diff.abs)
    val r = result.dom.valueInterval

    if (diff.lb >= 0) {
      ch |= v0.dom.intersectVal(i1 + r)
      ch |= v1.dom.intersectVal(i0 - r)
    } else if (diff.ub <= 0) {
      ch |= v0.dom.intersectVal(i1 - r)
      ch |= v1.dom.intersectVal(i0 + r)
    } else {
      ch |= unionInter(v0.dom, i0, i1 + r, i0, i1 - r)
      ch |= unionInter(v1.dom, i1, i0 - r, i1, i0 + r)
    }

    ch
  }

  private def unionInter(dom: Domain, i0: Interval, j0: Interval, i1: Interval, j1: Interval) =
    (i0 intersect j0, i1 intersect j1) match {
      case (Some(k0), Some(k1)) => dom.intersectVal(k0 union k1) | (
        if (k0.ub < k1.lb) dom.filter(i => k0.ub >= dom.value(i) || dom.value(i) >= k1.lb)
        else if (k1.ub < k0.lb) dom.filter(i => k1.ub >= dom.value(i) || dom.value(i) >= k0.lb)
        else false)

      case (Some(k0), None) => dom.intersectVal(k0)
      case (None, Some(k1)) => dom.intersectVal(k1)
      case (None, None) => throw UNSATException.e
    }

  override def revise(mod: Seq[Int]) = {
    val ch = fixPoint(shave())

    if (isBound) {
      assert(boundConsistent, this + " is not BC")
      assert(!super.revise(mod), this + " is not BC")
      entailCheck(ch)
      ch
    } else super.revise(mod) || ch
  }

  override def findSupport(position: Int, index: Int) =
    position match {
      case 0 => findValidTuple0(index);
      case 1 => findValidTupleV1(index);
      case 2 => findValidTupleV2(index);
      case _ => throw new IndexOutOfBoundsException;
    }

  def findValidTuple0(index: Int) = {
    val val0 = scope(0).dom.value(index)
    if (val0 < 0) None
    else {
      val dom1 = scope(1).dom
      val dom2 = scope(2).dom
      dom1.indices map { i =>
        (i, dom2.index(dom1.value(i) - val0))
      } find {
        case (_, j) => j >= 0 && dom2.present(j)
      } orElse {
        dom1.indices map { i =>
          (i, dom2.index(dom1.value(i) + val0))
        } find {
          case (_, j) => j >= 0 && dom2.present(j)
        }
      } map {
        case (i, j) => Array(index, i, j)
      }
    }
  }

  def findValidTupleV1(index: Int): Option[Array[Int]] = {
    val result = this.result.dom
    val value = scope(1).dom.value(index);
    val dom = scope(2).dom;
    dom.indices map { i =>
      (i, result.index(math.abs(value - dom.value(i))))
    } find {
      case (_, resIndex) => resIndex >= 0 && result.present(resIndex)
    } map {
      case (i, resIndex) => Array(resIndex, index, i)
    }
  }

  def findValidTupleV2(index: Int): Option[Array[Int]] = {
    val result = this.result.dom
    val value = scope(2).dom.value(index);
    val dom = scope(1).dom;
    dom.indices map { i =>
      (i, result.index(math.abs(value - dom.value(i))))
    } find {
      case (_, resIndex) => resIndex >= 0 && result.present(resIndex)
    } map {
      case (i, resIndex) => Array(resIndex, i, index)
    }
  }

  override def toString = result + " = |" + v0 + " - " + v1 + "|";

  def getEvaluation = if (isBound) 5 else {
    val d0 = result.dom.size
    val d1 = v0.dom.size
    val d2 = v1.dom.size
    d0 * d1 + d0 * d2 + d1 * d2;
  }

  def simpleEvaluation = 2
}
