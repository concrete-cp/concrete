package concrete.constraint.semantic;

import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.util.Interval
import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.BCCompanion

final class AddAC(val result: Variable, val v0: Variable, val v1: Variable, val skipIntervals: Boolean = false)
  extends Constraint(result, v0, v1) with Residues with BCCompanion with LazyLogging {

  def check(t: Array[Int]) = t(0) == t(1) + t(2)

  def findSupport(domains: IndexedSeq[Domain], position: Int, value: Int) = position match {
    case 0 => findValidTuple0(value, domains(1), domains(2));
    case 1 => findValidTupleV0(value, domains(0), domains(2));
    case 2 => findValidTupleV1(value, domains(0), domains(1));
  }

  private def findValidTuple0(val0: Int, dom1: Domain, dom2: Domain) = {
    dom1.map { v => (v, val0 - v) }.find {
      case (_, j) => dom2.present(j)
    } map {
      case (i, j) => Array(val0, i, j)
    }
  }

  private def findValidTupleV0(value: Int, result: Domain, dom: Domain) = {
    dom.map { v => (v, value + v) }
      .find {
        case (_, res) => result.present(res)
      } map {
        case (i, res) => Array(res, value, i)
      }
  }

  private def findValidTupleV1(value: Int, result: Domain, dom: Domain) = {
    dom.map { v => (v, value + v) }
      .find {
        case (_, res) => result.present(res)
      } map {
        case (i, res) => Array(res, i, value)
      }
  }

  override def toString(domains:IndexedSeq[Domain]) = domains(0) + " = " + domains(1) + " + " + domains(2)

  def getEvaluation(domains: IndexedSeq[Domain]) = if (skip(domains)) -1 else {
    val d0 = domains(0).size
    val d1 = domains(1).size
    val d2 = domains(2).size
    d0 * d1 + d0 * d2 + d1 * d2
  }

  def simpleEvaluation = 2
}
