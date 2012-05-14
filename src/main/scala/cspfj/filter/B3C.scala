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
package cspfj.filter;

import cspfj.Problem
import cspfj.Variable
import cspfj.util.Loggable
import scala.annotation.tailrec

/**
 * @author Julien VION
 *
 */
final class B3C(val problem: Problem) extends SingletonConsistency with Loggable {

  val subFilter = new AC3Constraint(problem)
  // private final static Logger logger =
  // Logger.getLogger("cspfj.filter.CDC");

  def singletonTest(variable: Variable) = {
    @tailrec
    def filterBound(index: Int, itr: Iterator[Int], changed: Boolean): Boolean = {
      if (itr.isEmpty) changed
      else if (check(variable, index)) {
        filterBound(itr.next, itr, true)
      } else false
    }

    val itr = variable.dom.indices
    var itr2 = variable.dom.indicesR
    filterBound(itr.next, itr, false) | filterBound(itr2.next, itr2, false)
  }

  override def toString = "3B w/ " + subFilter;

  override def getStatistics =
    Map("3B-singletonTests" -> nbSingletonTests) ++
      subFilter.getStatistics.map {
        case (k, v) => "3B-backend-" + k -> v
      }

}
