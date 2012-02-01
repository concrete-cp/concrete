package cspfj.constraint.semantic;

import cspfj.constraint.VariablePerVariable
import cspfj.problem.Domain
import cspfj.problem.Variable;
import cspfj.constraint.AbstractConstraint

/**
 * Constraint ax + b = y.
 *
 * @param a
 * @param x
 * @param b
 * @param y
 */
final class EqCached(val a: Int, val x: Variable, val b: Int, val y: Variable)
  extends AbstractConstraint(Array(x, y)) with VariablePerVariable {
  require(a != 0, "a must be != 0")

  val corresponding = Array(
    x.dom.allValues map { v => y.dom.index(a * v + b) },
    y.dom.allValues map { v =>
      val r = v - b
      if (r % a == 0) x.dom.index(r / a) else -1
    })

  /**
   * public
   * Constraint x = y.
   *
   * @param x
   * @param y
   */
  def this(x: Variable, y: Variable) = this(1, x, 0, y);

  def check = a * value(0) + b == value(1);

  def reviseVariable(position: Int) = {
    var change = false;
    val variable = scope(position);
    val otherVar = scope(1 - position);
    val correspond = corresponding(position);
    for (i <- variable.dom.indices) {
      val index = correspond(i);
      if (index < 0 || !otherVar.dom.present(index)) {
        variable.dom.remove(i)
        change = true
      }
    }
    change;
  }

  override def isConsistent(reviseCount: Int) = {
    val otherDom = y.dom
    val correspond = corresponding(0);
    x.dom.indices.exists { i =>
      val index = correspond(i)
      index >= 0 && otherDom.present(index)
    }
  }

  override def toString = (if (a != 1) a + "." else "") +
    x +
    (if (b > 0) " + " + b else if (b < 0) " - " + (-b) else "") +
    " = " + y

  def getEvaluation = scope(0).dom.size + scope(1).dom.size
}
