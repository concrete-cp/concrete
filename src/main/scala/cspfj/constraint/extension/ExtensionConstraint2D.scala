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

import cspfj.constraint.VariablePerVariable
import cspfj.constraint.AbstractConstraint
import cspfj.problem.Variable;
import cspfj.constraint.TupleEnumerator

final class ExtensionConstraint2D(
  scope: Array[Variable],
  matrix: Matrix2D,
  shared: Boolean) extends AbstractConstraint(scope) with VariablePerVariable with TupleEnumerator with ExtensionConstraint {

  private val GAIN_OVER_GENERAL = .1f;

  val matrixManager = new MatrixManager2D(scope, matrix, shared, tuple)

  override def getEvaluation =
    scope(0).dom.size * scope(1).dom.size * GAIN_OVER_GENERAL;

  def reviseVariable(position: Int) = {
    if (matrixManager.supportCondition(position)) {
      false
    } else {

      val variable = scope(position)

      // assert !variable.isAssigned();

      var revised = false;

      for (index <- variable.dom.indices if !matrixManager.hasSupport(position, index)) {
        variable.dom.remove(index);
        revised = true;
      }

      revised;
    }
  }

  def removeTuple(tuple: Array[Int]) = {
    disEntail();
    matrixManager.removeTuple(tuple);
  }

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

  def getMatrixManager = matrixManager

  def check = matrixManager.check

  override def toString = "ext2d(" + scope(0) + ", " + scope(1) + ")";

}
