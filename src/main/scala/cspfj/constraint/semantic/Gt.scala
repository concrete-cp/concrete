/**
 * CSPFJ - CSP solving API for Java
 * Copyright (C) 2006 Julien VION
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package cspfj.constraint.semantic;

import cspfj.constraint.Constraint
import cspfj.Domain
import cspfj.Variable;

/**
 * Constraint v0 + constant >(=) v1
 */
final class Gt(val v0: Variable, val constant: Int, val v1: Variable, val strict: Boolean)
  extends Constraint(Array(v0, v1)) {

  def this(v0: Variable, v1: Variable, strict: Boolean) =
    this(v0, 0, v1, strict);

  override def checkValues(t: Array[Int]) =
    if (strict) t(0) + constant > t(1);
    else t(0) + constant >= t(1);

  private def min(position: Int) = scope(position).dom.firstValue;

  private def max(position: Int) = scope(position).dom.lastValue

  private def removeGt(value: Int, position: Int) = {
    val dom = scope(position).dom
    val lb = if (strict) dom.closestGeq(value) else dom.closestGt(value)
    lb >= 0 && dom.removeFrom(lb)
  }

  private def removeLt(value: Int, position: Int) = {
    val dom = scope(position).dom;
    val ub = if (strict) dom.closestLeq(value) else dom.closestLt(value)
    ub >= 0 && dom.removeTo(ub)
  }

  def revise() = {
    assert(scope(0).dom.size > 0 && scope(1).dom.size > 0)

    val ch = removeLt(min(1) - constant, 0) | removeGt(max(0) + constant, 1)

    val max1 = max(1);
    val min0 = min(0) + constant;
    if (max1 < min0 || !strict && min0 == max1) {
      entail();
    }

    ch

  }

  override def isConsistent() = {
    val max0 = max(0) + constant;
    val min1 = min(1)
    max0 > min1 || !strict && max0 == min1;
  }

  override def toString = scope(0).toString + (
    if (constant > 0)
      " + " + constant
    else if (constant < 0)
      " - " + (-constant)
    else "") + (if (strict) " > " else " >= ") + scope(1)

  def advise(p: Int) = 2

  val simpleEvaluation = 1
}
