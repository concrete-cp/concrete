package cspfj.constraint.semantic;

import cspfj.constraint.VariablePerVariable
import cspfj.Domain
import cspfj.Variable
import cspfj.constraint.Constraint
import cspfj.util.Interval
import cspfj.constraint.Shaver

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

  def yIndex(xIndex: Int) =
    if (neg) y.dom.index(-x.dom.value(xIndex) + b)
    else y.dom.index(x.dom.value(xIndex) + b)

  def xIndex(yIndex: Int) = {
    val yv = y.dom.value(yIndex)
    if (neg) x.dom.index(b - yv) else x.dom.index(yv - b)
  }

  def shave(): List[Int] = {
    var mod: List[Int] = Nil
    if (neg) {
      if (scope(0).dom.intersectVal(scope(1).dom.valueInterval.negate + b)) {
        mod ::= 0
      }
      if (scope(1).dom.intersectVal(scope(0).dom.valueInterval.negate + b)) {
        mod ::= 1
      }
    } else {
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
    case 0 => x.dom.filter { i =>
      val index = yIndex(i)
      index >= 0 && y.dom.present(index)
    }

    case 1 => y.dom.filter { i =>
      val index = xIndex(i)
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

  override def toString = (if (neg) "-" else "") + x +
    (if (b > 0) " + " + b else if (b < 0) " - " + (-b) else "") +
    " == " + y

  def getEvaluation = if (x.dom.bound && y.dom.bound) 3 else (x.dom.size + y.dom.size)
  val simpleEvaluation = 2
}
