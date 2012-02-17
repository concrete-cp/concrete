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

import cspfj.constraint.DynamicConstraint
import cspfj.problem.Variable
import cspfj.constraint.AbstractConstraint
import cspfj.constraint.Residues
import cspfj.constraint.TupleEnumerator
import cspfj.UNSATException

final class ExtensionConstraintGeneral(matrix: Matrix, shared: Boolean, scope: Array[Variable])
  extends AbstractConstraint(scope)
  with ExtensionConstraint with DynamicConstraint with Residues with TupleEnumerator {

  val matrixManager = new MatrixManagerGeneral(scope, matrix, shared, tuple)

  def removeTuple(tuple: Array[Int]) = {
    disEntail();
    last.remove(tuple);
    matrixManager.removeTuple(tuple);
  }

  def check = matrixManager.check

  override def getType = super.getType + " w/ " + matrixManager.getType

  def removeTuples(base: Array[Int]) = {
    var removed = 0;
    tupleManager.setFirstTuple(base);
    do {
      if (removeTuple(this.tuple)) {
        removed += 1;
      }
    } while (tupleManager.setNextTuple(base));
    removed;
  }

  override def reviseVariable(position: Int) = {
    if (matrixManager.supportCondition(position)) {
      assert(try { super.reviseVariable(position); true }
      catch {
        case e: UNSATException => false
      })
      false;
    } else
      super.reviseVariable(position);
  }

}
