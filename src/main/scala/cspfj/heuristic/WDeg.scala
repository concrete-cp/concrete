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

package cspfj.heuristic;

import scala.collection.JavaConversions
import cspfj.constraint.Constraint
import cspfj.problem.Problem
import cspfj.problem.Variable;
import scala.annotation.tailrec

object WDeg {
  def wDeg(variable: Variable) = {
    val constraints = variable.constraints

    @tailrec
    def sum(i: Int, s: Int): Int =
      if (i < 0) s
      else {
        val c = constraints(i)
        if (c.isEntailed) sum(i - 1, s)
        else sum(i - 1, s + c.weight)
      }

    sum(constraints.size - 1, 0)
  }

  def nbUnboundVariables(constraint: Constraint) =
    constraint.scope.iterator.filter(_.dom.size > 1).size
}

final class WDeg(val problem: Problem) extends VariableHeuristic {

  def score(variable: Variable) = WDeg.wDeg(variable)

  override def toString = "max-wdeg"

}
