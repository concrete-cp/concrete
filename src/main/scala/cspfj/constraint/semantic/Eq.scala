package cspfj.constraint.semantic;

import cspfj.constraint.PVRConstraint
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
final class Eq(val a: Int, val x: Variable, val b: Int, val y: Variable)
  extends AbstractConstraint(Array(x, y)) with PVRConstraint {
  require(a != 0, "a must be != 0")

  val corresponding = Array(
    x.domain.allValues map { v => y.domain.index(a * v + b) },
    y.domain.allValues map { v =>
      val r = v - b
      if (r % a == 0) x.domain.index(r / a) else -1
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

  def revise(position: Int) = {
    var change = false;
    val variable = scope(position);
    val otherVar = scope(1 - position);
    val correspond = corresponding(position);
    for (i <- variable.domain) {
      val index = correspond(i);
      if (index < 0 || !otherVar.domain.present(index)) {
        variable.domain.remove(i)
        change = true
      }
    }
    change;
  }

  def isConsistent(reviseCount: Int) = {
    val otherDom = y.domain
    val correspond = corresponding(0);
    x.domain.exists { i =>
      val index = correspond(i)
      index >= 0 && otherDom.present(index)
    }
  }

  def toString = (if (a != 1) a + "." else "") +
    x +
    (if (b > 0) " + " + b else if (b < 0) " - " + (-b) else "") +
    " = " + y

  def getEvaluation = scope(0).domain.size + scope(1).domain.size
}
