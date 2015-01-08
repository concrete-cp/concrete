package concrete.constraint.semantic;

import concrete.Domain
import concrete.Variable
import concrete.constraint.BCCompanion
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.ProblemState

final class AbsDiffAC(val result: Variable, val v0: Variable, val v1: Variable, val skipIntervals: Boolean = false)
  extends Constraint(Array(result, v0, v1)) with Residues with BCCompanion {

  def check(t: Array[Int]) = t(0) == math.abs(t(1) - t(2))

  override def findSupport(ps: ProblemState, position: Int, value: Int) =
    position match {
      case 0 => findValidTupleResult(value, ps.dom(v0), ps.dom(v1));
      case 1 => findValidTupleV0(value, ps.dom(result), ps.dom(v1));
      case 2 => findValidTupleV1(value, ps.dom(result), ps.dom(v0));
      case _ => throw new IndexOutOfBoundsException;
    }

  def findValidTupleResult(val0: Int, dom1: Domain, dom2: Domain) = {
    if (val0 >= 0) {
      dom1.find { v => dom2.present(v - val0) }.map { v =>
        Array(val0, v, v - val0)
      } orElse dom1.find { v => dom2.present(v + val0) }.map { v =>
        Array(val0, v, v + val0)
      }
    } else None

  }

  def findValidTupleV0(value: Int, result: Domain, dom: Domain): Option[Array[Int]] = {

    dom.map { v =>
      (v, math.abs(value - v))
    } find {
      case (_, res) => result.present(res)
    } map {
      case (v, res) => Array(res, value, v)
    }
  }

  def findValidTupleV1(value: Int, result: Domain, dom: Domain): Option[Array[Int]] = {

    dom.map { v =>
      (v, math.abs(value - v))
    } find {
      case (_, res) => result.present(res)
    } map {
      case (v, res) => Array(res, v, value)
    }
  }

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} =AC= |${v0.toString(ps)} - ${v1.toString(ps)}|";

  def getEvaluation(ps: ProblemState) = {
    val d0 = ps.dom(result).size
    val d1 = ps.dom(v0).size
    val d2 = ps.dom(v1).size
    d0 * d1 + d0 * d2 + d1 * d2;
  }

  def simpleEvaluation = 2
}
