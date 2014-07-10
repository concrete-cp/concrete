package concrete.constraint.semantic;

import concrete.constraint.VariablePerVariable
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.util.Interval
import concrete.constraint.BC
import concrete.constraint.BCCompanion

final class SquareBC(val x: Variable, val y: Variable)
  extends Constraint(Array(x, y)) with BC {

  //  val corresponding = Array(
  //    x.dom.allValues map { v => y.dom.index(a * v + b) },
  //    y.dom.allValues map { v =>
  //      val r = v - b
  //      if (r % a == 0) x.dom.index(r / a) else -1
  //    })

  def checkValues(t: Array[Int]) = t(0) == t(1) * t(1)

  def shave() = {
    //val bounds = v0.dom.valueInterval * v1.dom.valueInterval - result.dom.valueInterval
    var mod: List[Int] = Nil
    if (x.dom.intersectVal(y.dom.valueInterval.sq)) {
      mod ::= 0
    }
    if (y.dom.intersectVal(x.dom.valueInterval.sqrt)) {
      mod ::= 1
    }
    mod
  }

  override def toString = s"$x == $y²"

  def advise(pos: Int) = 3 // else (x.dom.size + y.dom.size)
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

  def checkValues(t: Array[Int]) = t(0) == t(1) * t(1)

  def consistentX(xValue: Int) = {
    Square.sqrt(xValue).exists { root =>
      val idx = y.dom.index(root)
      idx >= 0 && y.dom.present(idx) || {
        val idx2 = y.dom.index(-root)
        idx2 >= 0 && y.dom.present(idx2)
      }
    }

  }

  def consistentY(yValue: Int) = {
    val idx = x.dom.index(yValue * yValue)
    idx >= 0 && x.dom.present(idx)
  }

  def reviseVariable(position: Int, mod: List[Int]) = position match {
    case 0 => x.dom.filterValues(consistentX)

    case 1 => y.dom.filterValues(consistentY)

    case _ => throw new AssertionError
  }

  override def isConsistent() = {
    x.dom.values.exists(consistentX) && y.dom.values.exists(consistentY)
  }

  override def toString = s"$x == $y²"

  def getEvaluation = if (x.dom.bound && y.dom.bound) 3 else (x.dom.size + y.dom.size)
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
}
