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

import cspfj.constraint.AbstractConstraint
import cspfj.problem.Domain
import cspfj.problem.Variable;

/**
 * Constraint v0 + constant >(=) v1
 */
final class Gt(val v0: Variable, val constant: Int, val v1: Variable, val strict: Boolean)
  extends AbstractConstraint(Array(v0, v1)) {

  def this(v0: Variable, v1: Variable, strict: Boolean) =
    this(v0, 0, v1, strict);

  override def check =
    if (strict)
      value(0) + constant > value(1);
    else value(0) + constant >= value(1);

  private def min(position: Int) = scope(position).dom.firstValue;

  private def max(position: Int) = scope(position).dom.lastValue

  private def removeGt(value: Int, position: Int) = {
    val dom = scope(position).dom
    val lb = if (strict) dom.closestGeq(value) else dom.closestGt(value)
    if (lb >= 0) dom.removeFrom(lb) > 0;
    else false
  }

  private def removeLt(value: Int, position: Int) = {
    val dom = scope(position).dom;
    val ub = if (strict) dom.closestLeq(value) else dom.closestLt(value)
    if (ub >= 0) dom.removeTo(ub) > 0;
    else false;

  }

  def revise() {
    assert(scope(0).dom.size > 0 && scope(1).dom.size > 0)

    removeLt(min(1) - constant, 0)
    removeGt(max(0) + constant, 1)
    val max1 = max(1);
    val min0 = min(0) + constant;
    if (max1 < min0 || !strict && min0 == max1) {
      entail();
    }

  }

  override def isConsistent() = {
    val max0 = max(0) + constant;
    val min1 = min(1)
    max0 > min1 || !strict && max0 == min1;
  }

  override def toString = scope(0).toString + (if (constant > 0)
    " + " + constant
  else if (constant < 0)
    " - " + (-constant)) + (if (strict) " > " else " >= ") + scope(1)

  override def getEvaluation =
    math.min(scope(0).dom.size, scope(1).dom.size);
  val simpleEvaluation = 1
}
