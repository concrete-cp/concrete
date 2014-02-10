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

import java.util.logging.Logger
import concrete.Problem
import concrete.Variable;
import cspom.Loggable

/**
 * @author Julien VION
 *
 */
final class SAC1(val problem: Problem) extends SingletonConsistency with Loggable {

  val subFilter = new ACC(problem)

  def singletonTest(variable: Variable) = {
    var changedGraph = false;
    for (index <- variable.dom.indices if variable.dom.size > 1) {
      if (Thread.interrupted) {
        throw new InterruptedException();
      }

      changedGraph |= check(variable, index)
    }
    changedGraph;
  }


  override def toString = "SAC-1 w/ " + subFilter;

}
