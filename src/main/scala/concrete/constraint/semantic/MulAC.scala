package concrete
package constraint
package semantic

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

  val simpleEvaluation = 2

  def check(t: Array[Int]): Boolean = t(0) == (t(1) * t(2))

  def findSupport(doms:Array[Domain], position: Int, value: Int): Option[Array[Int]] =
    position match {
      case 0 => findValidTupleResult(value, doms(1), doms(2));
      case 1 => findValidTupleV(doms(0), value, doms(2), 1);
      case 2 => findValidTupleV(doms(0), value, doms(1), 2);
      case _ => throw new IndexOutOfBoundsException()
    }

  private def findValidTupleResult(val0: Int, dom1: Domain, dom2: Domain): Option[Array[Int]] = {
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

  def findValidTupleV(result: Domain, value: Int, dom: Domain, of: Int): Option[Array[Int]] = {
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

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} =AC= ${v0.toString(ps)} * ${v1.toString(ps)}"

  def advise(ps: ProblemState, event: Event, pos: Int): Int = {
    val d0 = ps.card(result)
    val d1 = ps.card(v0)
    val d2 = ps.card(v1)
    val e = d0 * d1 + d0 * d2 + d1 * d2
    if (skip(ps, e)) -2 else e
  }
}
