package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.Domain
import concrete.Variable
import concrete.util.Interval
import concrete.constraint.BCCompanion

/**
 * Contrainte V0 = V1 * V2.
 *
 * @author vion
 *
 */
final class MulAC(val result: Variable, val v0: Variable, val v1: Variable, val skipIntervals: Boolean = false)
  extends Constraint(Array(result, v0, v1))
  with Residues
  with BCCompanion {

  def check(t: Array[Int]) = t(0) == (t(1) * t(2));

  def findSupport(domains: IndexedSeq[Domain], position: Int, value: Int) =
    position match {
      case 0 => findValidTuple0(value, domains(1), domains(2));
      case 1 => findValidTupleV(domains(0), value, domains(2), 1);
      case 2 => findValidTupleV(domains(0), value, domains(1), 2);
      case _ => throw new IndexOutOfBoundsException()
    }

  private def findValidTuple0(val0: Int, dom1: Domain, dom2: Domain): Option[Array[Int]] = {
    if (val0 == 0) {
      if (dom1.present(0)) {
        Some(Array(0, 0, dom2.head))
      } else if (dom2.present(0)) {
        Some(Array(0, dom1.head, 0))
      } else {
        None
      }
    } else {
      dom1
        .find(val1 => val1 != 0 && val0 % val1 == 0 && dom2.present(val0 / val1))
        .map(val1 => Array(val0, val1, val0 / val1))
    }

  }

  def findValidTupleV(result: Domain, value: Int, dom: Domain, of: Int) = {
    dom
      .find { v => result.present(value * v) }
      .map { v =>
        val a = new Array[Int](3)
        a(0) = value * v
        if (of == 1) {
          a(1) = value
          a(2) = v
        } else {
          a(1) = v
          a(2) = value
        }
        a
      }

  }

  override def toString(domains: IndexedSeq[Domain], s: State) =
    s"$result ${domains(0)} = $v0 ${domains(1)} * $v1 ${domains(2)}"

  def getEvaluation(dom: IndexedSeq[Domain]) = {
    val d0 = dom(0).size
    val d1 = dom(1).size
    val d2 = dom(2).size
    d0 * d1 + d0 * d2 + d1 * d2;
  }

  val simpleEvaluation = 2
}
