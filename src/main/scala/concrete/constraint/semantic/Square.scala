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

  def yIndex(xValue: Int) = {
    Square.sqrt(xValue) match {
      case Some(root) =>
        val idx = y.dom.index(root)
        if (idx >= 0) {
          idx
        } else {
          y.dom.index(-root)
        }
      case None => -1
    }

  }

  def xIndex(yValue: Int) = x.dom.index(yValue * yValue)

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
    case 0 => x.dom.filterValues { v =>
      val index = yIndex(v)
      index >= 0 && y.dom.present(index)
    }

    case 1 => y.dom.filterValues { v =>
      val index = xIndex(v)
      index >= 0 && x.dom.present(index)
    }

    case _ => throw new IllegalArgumentException
  }

  override def isConsistent() = {
    val otherDom = y.dom

    x.dom.indices.exists { i =>
      val index = yIndex(i)
      index >= 0 && otherDom.present(index)
    }
  }

  override def toString = s"$x == $y²"

  def getEvaluation = if (x.dom.bound && y.dom.bound) 3 else (x.dom.size + y.dom.size)
  val simpleEvaluation = 2
}

object Square {
  def sqrt(value: Int): Option[Int] = {
    val ds = math.sqrt(value).toInt
    if (ds * ds == value) {
      Some(ds)
    } else {
      None
    }
  }
}
