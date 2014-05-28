package concrete.constraint.semantic;

import concrete.constraint.VariablePerVariable
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.util.Interval
import concrete.constraint.Shaver

/**
 * Constraint x = y².
 *
 * @param x
 * @param y
 */
final class Square(val x: Variable, val y: Variable)
  extends Constraint(Array(x, y)) with VariablePerVariable {

  //  val corresponding = Array(
  //    x.dom.allValues map { v => y.dom.index(a * v + b) },
  //    y.dom.allValues map { v =>
  //      val r = v - b
  //      if (r % a == 0) x.dom.index(r / a) else -1
  //    })

  def checkValues(t: Array[Int]) = t(0) == t(1) * t(1)

  def consistentX(xValue: Int) = {
    Square.sqrt(xValue) match {
      case Some(root) =>
        val idx = y.dom.index(root)
        if (idx >= 0 && y.dom.present(idx)) {
          true
        } else {
          val idx2 = y.dom.index(-root)
          idx2 >= 0 && y.dom.present(idx2)
        }
      case None => false
    }

  }

  def consistentY(yValue: Int) = {
    val idx = x.dom.index(yValue * yValue)
    idx >= 0 && x.dom.present(idx)
  }

  //  def shave(): List[Int] = {
  //    var mod: List[Int] = Nil
  //    if (neg) {
  //      if (scope(0).dom.intersectVal(scope(1).dom.valueInterval.negate + b)) {
  //        mod ::= 0
  //      }
  //      if (scope(1).dom.intersectVal(scope(0).dom.valueInterval.negate + b)) {
  //        mod ::= 1
  //      }
  //    } else {
  //      if (scope(0).dom.intersectVal(scope(1).dom.valueInterval - b)) {
  //        mod ::= 0
  //      }
  //      if (scope(1).dom.intersectVal(scope(0).dom.valueInterval + b)) {
  //        mod ::= 1
  //      }
  //    }
  //    mod
  //  }

  def reviseVariable(position: Int, mod: List[Int]) = position match {
    case 0 => x.dom.filterValues(consistentX)

    case 1 => y.dom.filterValues(consistentY)

    case _ => throw new IllegalArgumentException
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
