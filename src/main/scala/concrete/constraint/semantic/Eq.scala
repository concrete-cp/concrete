package concrete.constraint.semantic;

import concrete.constraint.VariablePerVariable
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.util.Interval
import concrete.constraint.Shaver

/**
 * Constraint (-)x + b = y.
 *
 * @param a
 * @param x
 * @param b
 * @param y
 */
final class Eq(val neg: Boolean, val x: Variable, val b: Int, val y: Variable)
  extends Constraint(Array(x, y)) with Shaver {

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

  def checkValues(t: Array[Int]) = (if (neg) -t(0) else t(0)) + b == t(1);

  private def yValue(x: Int) =
    if (neg) -x + b else x + b

  private def xValue(y: Int) =
    if (neg) b - y else y - b
  //
  //  def yIndex(xIndex: Int) =
  //    if (neg) y.dom.index(-x.dom.value(xIndex) + b)
  //    else y.dom.index(x.dom.value(xIndex) + b)
  //
  //  def xIndex(yIndex: Int) = {
  //    val yv = y.dom.value(yIndex)
  //    if (neg) x.dom.index(b - yv) else x.dom.index(yv - b)
  //  }

  def shave(): List[Int] = {
    var mod: List[Int] = Nil
    if (neg) {
      // -x + b = y <=> x = -y + b 
      if (scope(0).dom.intersectVal(scope(1).dom.valueInterval.negate + b)) {
        mod ::= 0
      }
      if (scope(1).dom.intersectVal(scope(0).dom.valueInterval.negate + b)) {
        mod ::= 1
      }
    } else {
      // x + b = y <=> x = y - b
      if (scope(0).dom.intersectVal(scope(1).dom.valueInterval - b)) {
        mod ::= 0
      }
      if (scope(1).dom.intersectVal(scope(0).dom.valueInterval + b)) {
        mod ::= 1
      }
    }
    mod
  }

  def reviseVariable(position: Int, mod: List[Int]) = position match {
    case 0 => x.dom.filterValues { xv =>
      y.dom.presentVal(yValue(xv))
    }

    case 1 => y.dom.filterValues { yv =>
      x.dom.presentVal(xValue(yv))
    }

    case _ => throw new IllegalArgumentException
  }

  override def isConsistent() = {
    x.dom.values.exists { xv =>
      y.dom.presentVal(yValue(xv))
    }
  }

  override def toString = (if (neg) "-" else "") + x +
    (if (b > 0) " + " + b else if (b < 0) " - " + (-b) else "") +
    " == " + y

  def getEvaluation = if (x.dom.bound && y.dom.bound) 3 else (x.dom.size + y.dom.size)
  val simpleEvaluation = 2
}
