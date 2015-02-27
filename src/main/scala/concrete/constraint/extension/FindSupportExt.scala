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

package concrete.constraint.extension;

import concrete.Variable
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import cspom.UNSATException
import concrete.constraint.Constraint
import concrete.Domain
import concrete.ProblemState

final class FindSupportExt(scope: Array[Variable], tts: TupleTrieSet, shared: Boolean)
  extends ConflictCount(scope, tts, shared) with Residues {

  private val rel = tts.reduceable

  override def findSupport(ps: ProblemState, p: Int, i: Int) = {
    val s = rel.findSupport(scope.map(ps.dom).toIndexedSeq, p, i)

    assert(s.forall(rel.contains))

    s
  }

  private val edges = rel.edges

  override def getEvaluation(ps: ProblemState) = edges

  override def check(tuple: Array[Int]) = rel.contains(tuple)

  def removeTuple(t: Array[Int]) = throw new UnsupportedOperationException
  def removeTuples(t: Array[Int]) = throw new UnsupportedOperationException

  override def simpleEvaluation = 6

  override def dataSize = rel.edges
}
