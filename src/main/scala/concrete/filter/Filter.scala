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

package concrete.filter;

import concrete.constraint.Constraint
import concrete.Variable
import concrete.Problem
import concrete.ProblemState
import concrete.Outcome

/**
 * @author scand1sk
 *
 */
trait Filter {

  def problem: Problem

  /**
   * @return false iff an inconsistency has been detected
   */
  def reduceAll(states: ProblemState): Outcome

  def reduceAfter(constraints: Iterable[Constraint], states: ProblemState): Outcome

  def reduceAfter(variable: Variable, states: ProblemState): Outcome

}
