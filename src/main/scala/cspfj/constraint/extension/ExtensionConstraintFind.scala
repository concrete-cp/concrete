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

package cspfj.constraint.extension;

import cspfj.Variable
import cspfj.constraint.Residues
import cspfj.constraint.TupleEnumerator
import cspfj.UNSATException
import cspfj.constraint.Constraint

final class ExtensionConstraintFind(scope: Array[Variable], tts: TupleTrieSet, shared: Boolean)
  extends ConflictCount(scope, tts, shared) with Residues {

  val rel = tts.reduceable

  override def findSupport(p: Int, i: Int) = rel.find(
    (depth, index) => if (depth == p) i == index else scope(depth).dom.present(index))

  override val getEvaluation = rel.nodes

  override def checkIndices(tuple: Array[Int]) = rel.contains(tuple)

  def removeTuple(t: Array[Int]) = throw new UnsupportedOperationException
  def removeTuples(t: Array[Int]) = throw new UnsupportedOperationException

  override def simpleEvaluation = 6
}
