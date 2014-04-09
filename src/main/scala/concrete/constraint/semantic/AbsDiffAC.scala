package concrete.constraint.semantic;

import concrete.constraint.Residues
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.UNSATException
import concrete.util.Interval
import concrete.constraint.Shaver
import concrete.UNSATObject
import concrete.constraint.BCCompanion

final class AbsDiffAC(val result: Variable, val v0: Variable, val v1: Variable, val skipIntervals: Boolean = false)
  extends Constraint(Array(result, v0, v1)) with Residues with BCCompanion {

  def checkValues(t: Array[Int]) = t(0) == math.abs(t(1) - t(2))

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

      dom1.indices.find { i => dom2.presentVal(dom1.value(i) - val0) }.map { i =>
        Array(index, i, dom2.index(dom1.value(i) - val0))
      } orElse dom1.indices.find { i => dom2.presentVal(dom1.value(i) + val0) }.map { i =>
        Array(index, i, dom2.index(dom1.value(i) + val0))
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

  def getEvaluation = {
    val d0 = result.dom.size
    val d1 = v0.dom.size
    val d2 = v1.dom.size
    d0 * d1 + d0 * d2 + d1 * d2;
  }

  def simpleEvaluation = 2
}
