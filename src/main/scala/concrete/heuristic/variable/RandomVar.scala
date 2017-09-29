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

import scala.util.Random

final class RandomVar(val pool: Seq[Variable], params: ParameterManager, val trustCandidates: Boolean = true)
  extends VariableHeuristic with LazyLogging {

  val rand: Random = {
    val seed = params.getOrElse("randomseed", 0L) + params.getOrElse("iteration", 0)
    new Random(seed)
  }
  private val poolSet = pool.toSet

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def toString = "random-var"

  def select(state: ProblemState, i: Seq[Variable]): Option[Variable] = {
    val s = if (trustCandidates) {
      i
    } else {
      i.filter(poolSet)
    }

    if (s.isEmpty) {
      None
    } else {
      logger.info("Select randomly")
      Some(s(rand.nextInt(s.size)))
    }

  }

  override def shouldRestart: Boolean = true

  def event(e: EventObject): Unit = ()
}
