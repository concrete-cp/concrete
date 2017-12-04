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

package concrete
package heuristic
package variable

import java.util.EventObject

import com.typesafe.scalalogging.LazyLogging

final class LexVar(val pool: Seq[Variable]) extends VariableHeuristic with LazyLogging {

  private val order = pool.zipWithIndex.toMap

  override def select(state: ProblemState, i: Seq[Variable]): Option[Variable] = {
    var bestVariable: Variable = null
    var bestIndex = pool.size
    for (v <- i) {
      val index = order.getOrElse(v, bestIndex)
      if (index < bestIndex) {
        bestVariable = v
        bestIndex = index
      }
    }
    logger.info(s"Selected first element $bestVariable")
    Option(bestVariable)
  }


  override def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def toString = "lex-var"

  override def shouldRestart = false

  def event[S <: Outcome](e: EventObject, ps: S): S = ps
}
