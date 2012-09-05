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
import cspfj.Variable
import cspfj.constraint.TupleEnumerator
import scala.annotation.tailrec
import cspfj.Statistic
import cspfj.StatisticsManager

object ExtensionConstraint2D {
  @Statistic
  var checks = 0;
  @Statistic
  var presenceChecks = 0
}

final class ExtensionConstraint2D(
  scope: Array[Variable],
  private var matrix2d: Matrix2D,
  shared: Boolean)
  extends ConflictCount(scope, matrix2d, shared)
  with VariablePerVariable {

  private val GAIN_OVER_GENERAL = 3;

  private val MINIMUM_SIZE_FOR_LAST = 3 * java.lang.Long.SIZE;

  /**
   * No need for "last" data structure if domain sizes <=
   * MINIMUM_SIZE_FOR_LAST
   */
  private val residues =
    if (scope.map(_.dom.maxSize).max > MINIMUM_SIZE_FOR_LAST) {
      Array(new Array[Int](scope(0).dom.maxSize), new Array[Int](scope(1).dom.maxSize))
    } else null

  override def getEvaluation = (scope(0).dom.size * scope(1).dom.size) / GAIN_OVER_GENERAL

  override def simpleEvaluation = 2

  def reviseVariable(position: Int, mod: List[Int]) =
    !supportCondition(position) && scope(position).dom.filter(i => hasSupport(position, i))

  def removeTuple(tuple: Array[Int]) = {
    disEntail();
    set(tuple, false)
  }

  def removeTuples(base: Array[Int]) = tuples(base).count(removeTuple)

  override def toString = "ext2d(" + scope(0) + ", " + scope(1) + ")" + (if (isEntailed) " [entailed]" else "");

  def hasSupport(variablePosition: Int, index: Int) =
    if (residues == null) hasSupportNR(variablePosition, index);
    else hasSupportR(variablePosition, index);

  private def hasSupportR(variablePosition: Int, index: Int) = {
    controlResidue(variablePosition, index) || {
      val matrixBV = matrix2d.getBitVector(variablePosition, index);
      val intersection = scope(1 - variablePosition).dom.intersects(matrixBV)

      if (intersection >= 0) {
        ExtensionConstraint2D.checks += 1 + intersection;
        residues(variablePosition)(index) = intersection;
        true;
      } else {
        ExtensionConstraint2D.checks += matrixBV.realSize;
        false;
      }
    }
  }

  private def hasSupportNR(variablePosition: Int, index: Int) = {
    val matrixBV = matrix2d.getBitVector(variablePosition, index);
    val intersection = scope(1 - variablePosition).dom.intersects(matrixBV)

    if (intersection >= 0) {
      ExtensionConstraint2D.checks += 1 + intersection;
      true;
    } else {
      ExtensionConstraint2D.checks += matrixBV.realSize;
      false;
    }
  }

  private def controlResidue(position: Int, index: Int) = {
    val part = residues(position)(index)
    ExtensionConstraint2D.presenceChecks += 1
    (part != -1 && scope(1 - position).dom.intersects(
      matrix2d.getBitVector(position, index), part))
  }

  override def checkIndices(t: Array[Int]) = matrix.check(t)

  override def unshareMatrix() = {
    matrix2d = super.unshareMatrix().asInstanceOf[Matrix2D]
    matrix2d
  }

}
