package concrete.constraint.semantic;

import concrete.constraint.VariablePerVariable
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.util.Interval
import concrete.constraint.BC
import concrete.constraint.BCCompanion
import concrete.constraint.Removals
import concrete.ProblemState
import concrete.Revised
import concrete.constraint.StatelessBC

/**
 * Constraint (-)x + b = y.
 *
 * @param a
 * @param x
 * @param b
 * @param y
 */
final class EqAC(val neg: Boolean, val x: Variable, val b: Int, val y: Variable)
  extends Constraint(Array(x, y)) with VariablePerVariable with BCCompanion {
  def this(x: Variable, y: Variable) = this(false, x, 0, y);

  private def yValue(x: Int) =
    if (neg) -x + b else x + b

  private def xValue(y: Int) =
    if (neg) b - y else y - b

  def check(t: Array[Int]) = (if (neg) -t(0) else t(0)) + b == t(1);

  def skipIntervals: Boolean = true

  def simpleEvaluation: Int = 2

  def getEvaluation(domains: IndexedSeq[Domain]): Int = if (skip(domains)) -1 else domains(0).size + domains(1).size

  override def isConsistent(domains: IndexedSeq[Domain]) = {
    domains(0).exists { xv =>
      domains(1).present(yValue(xv))
    }
  }

  def reviseVariable(domains: IndexedSeq[Domain], position: Int, mod: List[Int]) = {
    position match {

      case 0 => domains(0).filter { xv =>
        domains(1).present(yValue(xv))
      }

      case 1 => domains(1).filter { yv =>
        domains(0).present(xValue(yv))
      }

      case _ => throw new IllegalArgumentException
    }
  }

  override def toString = (if (neg) "-" else "") + x +
    (if (b > 0) " + " + b else if (b < 0) " - " + (-b) else "") +
    " =AC= " + y
}

final class EqBC(val neg: Boolean, val x: Variable, val b: Int, val y: Variable)
  extends Constraint(Array(x, y)) with StatelessBC {

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

  def shave(domains: IndexedSeq[Domain]) = {
    var mod: List[Int] = Nil
    if (neg) {
      // -x + b = y <=> x = -y + b 
      Revised(Vector(
        domains(0) & (-domains(1).span + b),
        domains(1) & (-domains(0).span + b)))
    } else {
      // x + b = y <=> x = y - b
      Revised(Vector(
        domains(0) & (domains(1).span - b),
        domains(1) & (domains(0).span + b)))
    }
  }

  def isConsistent(domains: IndexedSeq[Domain]) = domains(0).span intersects domains(1).span

  override def toString = (if (neg) "-" else "") + x +
    (if (b > 0) " + " + b else if (b < 0) " - " + (-b) else "") +
    " =BC= " + y

  def advise(domains: IndexedSeq[Domain],p: Int) = 3
  val simpleEvaluation = 2
}
