/**
 * CSPFJ - CSP solving API for Java)
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

package cspfj;

import cspfj.filter.ACC
import cspfj.filter.Filter

object DummySolver {
  @Parameter("dummy.filter")
  var filterClass: Class[_ <: Filter] = classOf[ACC];

  ParameterManager.register(this);
}

final class DummySolver(prob: Problem) extends Solver(prob) {

  val filter = DummySolver.filterClass.getConstructor(classOf[Problem]).newInstance(problem);
  statistics.register("filter", filter);

  def nextSolution() = if (preprocess(filter)) UNKNOWNResult else UNSAT

  override def toString = "dummy"

  def reset() {}

}
