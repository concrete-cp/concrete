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

package concrete.constraint.extension

import concrete.constraint.Residues
import concrete.{Domain, Event, ProblemState, Variable}

final class FindSupportExt(scope: Array[Variable], tts: Relation)
  extends ExtensionConstraint(scope) with ConflictCount with Residues {

  private val edges = tts.edges
  var matrix: Matrix = ??? //new MDDMatrix(tts, false)

  override def findSupport(doms: Array[Domain], p: Int, i: Int): Option[Array[Int]] = {
    val s = tts.findSupport(doms, p, i)

    assert(s.forall(tts.contains))

    s
  }

  override def advise(ps: ProblemState, event: Event, pos: Int): Int = edges

  override def check(tuple: Array[Int]): Boolean = tts.contains(tuple)

  def removeTuple(t: Array[Int]) = throw new UnsupportedOperationException

  def removeTuples(t: Array[Int]) = throw new UnsupportedOperationException

  override def simpleEvaluation = 6

  override def dataSize: Int = tts.edges
}
