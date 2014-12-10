package concrete.constraint.semantic;

import concrete.constraint.Residues
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.UNSATException
import concrete.util.Interval
import concrete.UNSATObject
import concrete.constraint.BCCompanion

final class AbsDiffAC(val result: Variable, val v0: Variable, val v1: Variable, val skipIntervals: Boolean = false)
  extends Constraint(Array(result, v0, v1)) with Residues with BCCompanion {

  def check(t: Array[Int]) = t(0) == math.abs(t(1) - t(2))

  override def findSupport(domains: IndexedSeq[Domain], position: Int, value: Int) =
    position match {
      case 0 => findValidTuple0(value, domains(1), domains(2));
      case 1 => findValidTupleV1(value, domains(0), domains(2));
      case 2 => findValidTupleV2(value, domains(0), domains(1));
      case _ => throw new IndexOutOfBoundsException;
    }

  def findValidTuple0(val0: Int, dom1: Domain, dom2: Domain) = {
    if (val0 >= 0) {
      dom1.find { v => dom2.present(v - val0) }.map { v =>
        Array(val0, v, v - val0)
      } orElse dom1.find { v => dom2.present(v + val0) }.map { v =>
        Array(val0, v, v + val0)
      }
    } else None

  }

  def findValidTupleV1(value: Int, result: Domain, dom: Domain): Option[Array[Int]] = {

    dom.map { v =>
      (v, math.abs(value - v))
    } find {
      case (_, res) => result.present(res)
    } map {
      case (v, res) => Array(res, value, v)
    }
  }

  def findValidTupleV2(value: Int, result: Domain, dom: Domain): Option[Array[Int]] = {

    dom.map { v =>
      (v, math.abs(value - v))
    } find {
      case (_, res) => result.present(res)
    } map {
      case (v, res) => Array(res, v, value)
    }
  }

  override def toString(domains: IndexedSeq[Domain], s: State) =
    s"$result ${domains(0)} =AC= |$v0 ${domains(1)} - $v1 ${domains(2)}|";

  def getEvaluation(domains: IndexedSeq[Domain]) = {
    val d0 = domains(0).size
    val d1 = domains(1).size
    val d2 = domains(2).size
    d0 * d1 + d0 * d2 + d1 * d2;
  }

  def simpleEvaluation = 2
}
