package concrete.constraint.semantic;

import concrete.constraint.VariablePerVariable
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.util.Interval
import concrete.constraint.BC
import concrete.constraint.BCCompanion
import concrete.Revised
import concrete.constraint.StatelessBC
import concrete.constraint.Stateless

final class SquareBC(val x: Variable, val y: Variable)
  extends Constraint(Array(x, y)) with StatelessBC {

  //  val corresponding = Array(
  //    x.dom.allValues map { v => y.dom.index(a * v + b) },
  //    y.dom.allValues map { v =>
  //      val r = v - b
  //      if (r % a == 0) x.dom.index(r / a) else -1
  //    })

  def check(t: Array[Int]) = t(0) == t(1) * t(1)

  def shave(domains: IndexedSeq[Domain]) = {
    Revised(IndexedSeq(domains(0) & domains(1).span.sq, domains(1) & domains(0).span.sqrt))
  }

  override def toString(domains: IndexedSeq[Domain]) = s"${domains(0)} == ${domains(1)}²"

  def advise(domains: IndexedSeq[Domain], pos: Int) = 3 // else (x.dom.size + y.dom.size)
  val simpleEvaluation = 2
}

/**
 * Constraint x = y².
 *
 * @param x
 * @param y
 */
final class SquareAC(val x: Variable, val y: Variable)
  extends Constraint(Array(x, y)) with BCCompanion with VariablePerVariable {

  def skipIntervals = false
  //  val corresponding = Array(
  //    x.dom.allValues map { v => y.dom.index(a * v + b) },
  //    y.dom.allValues map { v =>
  //      val r = v - b
  //      if (r % a == 0) x.dom.index(r / a) else -1
  //    })

  def check(t: Array[Int]) = t(0) == t(1) * t(1)

  def consistentX(xValue: Int, yDomain: Domain) = {
    Square.sqrt(xValue).exists { root =>
      yDomain.present(root) || yDomain.present(-root)
    }

  }

  def consistentY(yValue: Int, xDomain: Domain) = {
    math.abs(yValue) < Square.MAX_SQUARE && xDomain.present(yValue * yValue)
  }

  def reviseVariable(domains: IndexedSeq[Domain], position: Int, mod: List[Int]) = position match {
    case 0 => domains(0).filter(v => consistentX(v, domains(1)))

    case 1 => domains(1).filter(v => consistentY(v, domains(0)))

    case _ => throw new AssertionError
  }

  override def isConsistent(domains: IndexedSeq[Domain]) = {
    domains(0).exists(v => consistentX(v, domains(1))) && domains(1).exists(v => consistentY(v, domains(0)))
  }

  override def toString(domains: IndexedSeq[Domain]) = s"${domains(0)} == ${domains(1)}²"

  def getEvaluation(domains: IndexedSeq[Domain]) = domains(0).size + domains(1).size
  val simpleEvaluation = 2
}

object Square {
  def sqrt(x: Int): Option[Int] = {
    if ((x & 2) == 2 || (x & 7) == 5) {
      None
    } else if ((x & 11) == 8 || (x & 31) == 20) {
      None
    } else if ((x & 47) == 32 || (x & 127) == 80) {
      None
    } else if ((x & 191) == 128 || (x & 511) == 320) {
      None
    } else {
      val root = Math.sqrt(x).toInt
      if (root * root == x) {
        Some(root)
      } else {
        None
      }

    }
  }

  val MAX_SQUARE = 46340 // sqrt(Int.MaxValue)
}
