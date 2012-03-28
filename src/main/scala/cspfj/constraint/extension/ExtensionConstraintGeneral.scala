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

import cspfj.problem.Variable
import cspfj.constraint.Residues
import cspfj.constraint.TupleEnumerator
import cspfj.UNSATException

final class ExtensionConstraintGeneral(
  private var _matrix: Matrix,
  private var shared: Boolean, scope: Array[Variable])
  extends ConflictCount(scope) with Residues {

  def removeTuple(tuple: Array[Int]) = {
    disEntail();
    last.remove(tuple);
    unshareMatrix()
    if (matrix.check(tuple)) {
      matrix.set(tuple, false)
      true
    } else false

  }

  def matrix = _matrix

  def unshareMatrix() = {
    if (shared) {
      _matrix = matrix.copy
      shared = false
    }
  }

  def removeTuples(base: Array[Int]) = tuples(base).count(removeTuple)

  override def reviseVariable(position: Int, mod: Seq[Int]) = {
    if (supportCondition(position)) {
      assert(!super.reviseVariable(position, mod))
      false
    } else
      super.reviseVariable(position, mod);
  }

}
