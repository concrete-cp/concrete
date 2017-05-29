package concrete.constraint.semantic

;

import concrete.constraint.{BCCompanion, Constraint, Residues}
import concrete.{Domain, Event, ProblemState, Variable}

final class AbsDiffAC(val result: Variable, val v0: Variable, val v1: Variable, val skipIntervals: Boolean = false)
  extends Constraint(Array(result, v0, v1)) with Residues with BCCompanion {

  def check(t: Array[Int]) = t(0) == math.abs(t(1) - t(2))

  override def findSupport(doms: Array[Domain], position: Int, value: Int) =
    position match {
      case 0 => findValidTupleResult(value, doms(1), doms(2));
      case 1 => findValidTupleV0(value, doms(0), doms(2));
      case 2 => findValidTupleV1(value, doms(0), doms(1));
      case _ => throw new IndexOutOfBoundsException;
    }

  def findValidTupleResult(val0: Int, dom1: Domain, dom2: Domain) = {
    if (val0 >= 0) {
      dom1
        .find { v => dom2.present(v - val0) }
        .map { v =>
          Array(val0, v, v - val0)
        }
        .orElse {
          dom1
            .find { v => dom2.present(v + val0) }
            .map { v =>
              Array(val0, v, v + val0)
            }
        }
    } else None

  }

  def findValidTupleV0(value: Int, result: Domain, dom: Domain): Option[Array[Int]] = {

    dom.view
      .map { v =>
        (v, math.abs(value - v))
      }
      .find {
        case (_, res) => result.present(res)
      }
      .map {
        case (v, res) => Array(res, value, v)
      }
  }

  def findValidTupleV1(value: Int, result: Domain, dom: Domain): Option[Array[Int]] = {

    dom.view
      .map { v =>
        (v, math.abs(value - v))
      }
      .find {
        case (_, res) => result.present(res)
      }
      .map {
        case (v, res) => Array(res, v, value)
      }
  }

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} =AC= |${v0.toString(ps)} - ${v1.toString(ps)}|";

  def advise(ps: ProblemState, event: Event, position: Int) = {
    val d0 = ps.dom(result).size
    val d1 = ps.dom(v0).size
    val d2 = ps.dom(v1).size
    val eval = d0 * d1 + d0 * d2 + d1 * d2
    if (skip(ps, eval)) {
      -2
    } else {
      eval
    }
  }

  def simpleEvaluation = 2
}
