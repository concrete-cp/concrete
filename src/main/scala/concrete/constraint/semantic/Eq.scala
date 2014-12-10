package concrete.constraint.semantic;

import com.typesafe.scalalogging.LazyLogging

import concrete.Contradiction
import concrete.Domain
import concrete.Revised
import concrete.Variable
import concrete.constraint.BC
import concrete.constraint.BCCompanion
import concrete.constraint.Constraint
import concrete.constraint.Removals

/**
 * Constraint (-)x + b = y.
 *
 * @param a
 * @param x
 * @param b
 * @param y
 */
final class EqAC(val neg: Boolean, val x: Variable, val b: Int, val y: Variable)
  extends Constraint(Array(x, y)) with Removals with BCCompanion {
  def this(x: Variable, y: Variable) = this(false, x, 0, y);

  type State = Unit
  def initState = Unit

  private def yValue(x: Int) =
    if (neg) -x + b else x + b

  private def xValue(y: Int) =
    if (neg) b - y else y - b

  def check(t: Array[Int]) = (if (neg) -t(0) else t(0)) + b == t(1);

  def skipIntervals: Boolean = true

  def simpleEvaluation: Int = 2

  def getEvaluation(domains: IndexedSeq[Domain]): Int = if (skip(domains)) -1 else domains(0).size + domains(1).size

  override def isConsistent(domains: IndexedSeq[Domain], s: State) = {
    domains(0).exists { xv =>
      domains(1).present(yValue(xv))
    }
  }

  def revise(domains: IndexedSeq[Domain], modified: List[Int], state: State) = {
    val s = skip(modified)
    val d0 = if (s == 0) { domains(0) } else {
      domains(0).filter { xv =>
        domains(1).present(yValue(xv))
      }
    }

    if (d0.isEmpty) {
      Contradiction
    } else {

      val d1 = if (s == 1) { domains(1) } else {
        domains(1).filter { yv =>
          domains(0).present(xValue(yv))
        }
      }

      Revised(Vector(d0, d1), d0.size == 1)

    }
  }

  override def toString(domains: IndexedSeq[Domain], s: State) = s"${if (neg) "-" else ""}$x ${domains(0)}${
    if (b > 0) " + " + b else if (b < 0) " - " + (-b) else ""
  } =AC= $y ${domains(1)}"
}

final class EqBC(val neg: Boolean, val x: Variable, val b: Int, val y: Variable)
  extends Constraint(Array(x, y)) with BC with LazyLogging {
  type State = Unit
  def initState = Unit
  //  val corresponding = Array(
  //    x.dom.allValues map { v => y.dom.index(a * v + b) },
  //    y.dom.allValues map { v =>
  //      val r = v - b
  //      if (r % a == 0) x.dom.index(r / a) else -1
  //    })

  /**
   * public
   * Constraint x = y.
   *
   * @param x
   * @param y
   */
  def this(x: Variable, y: Variable) = this(false, x, 0, y);

  def check(t: Array[Int]) = (if (neg) -t(0) else t(0)) + b == t(1);

  def shave(domains: IndexedSeq[Domain], s: State) = {
    var mod: List[Int] = Nil
    val nd = if (neg) {
      // -x + b = y <=> x = -y + b 
      Array(
        domains(0) & (-domains(1).span + b),
        domains(1) & (-domains(0).span + b))

    } else {
      // x + b = y <=> x = y - b
      Array(
        domains(0) & (domains(1).span - b),
        domains(1) & (domains(0).span + b))

    }
    Revised(nd, isFree(nd))
  }

  override def isConsistent(domains: IndexedSeq[Domain], s: State) = {
    domains(0).span intersects domains(1).span
  }

  override def toString(domains: IndexedSeq[Domain], s: State) = s"${if (neg) "-" else ""}$x ${domains(0)}${
    if (b > 0) " + " + b else if (b < 0) " - " + (-b) else ""
  } =BC= $y ${domains(1)}"

  def advise(domains: IndexedSeq[Domain], p: Int) = 3
  val simpleEvaluation = 2
}
