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
import concrete.constraint.VariablePerVariable
import concrete.util.BitVector
import cspom.Statistic
import concrete.ProblemState
import concrete.Domain

object BinaryExt {
  @Statistic
  var checks = 0;
  @Statistic
  var presenceChecks = 0

  val MINIMUM_SIZE_FOR_LAST = 3 * java.lang.Long.SIZE;

  /**
   * No need for residues if domain sizes <= MINIMUM_SIZE_FOR_LAST
   */
  def apply(scope: Array[Variable], matrix2d: Matrix2D, shared: Boolean) = {
    if (scope.map(_.initDomain.size).max > MINIMUM_SIZE_FOR_LAST) {
      new BinaryExtR(scope, matrix2d, shared)
    } else {
      new BinaryExtNR(scope, matrix2d, shared)
    }
  }
}

abstract class BinaryExt(
  scope: Array[Variable],
  private var matrix2d: Matrix2D,
  shared: Boolean)
  extends ConflictCount(scope, matrix2d, shared)
  with VariablePerVariable {

  require(scope.forall(_.initDomain.head >= 0))

  private val GAIN_OVER_GENERAL = 3;

  override def getEvaluation(domains: IndexedSeq[Domain]) = (domains(0).size * domains(1).size) / GAIN_OVER_GENERAL

  override def simpleEvaluation = 2

  def reviseVariable(domains: IndexedSeq[Domain], position: Int, mod: List[Int]) =
    if (supportCondition(domains, position)) { domains(position) }
    else { domains(position).filter(i => hasSupport(domains, position, i)) }

  def removeTuple(tuple: Array[Int]) = {
    ??? //disEntail()
    set(tuple, false)
  }

  def removeTuples(base: Array[Int]) = ??? //tuples(base).count(removeTuple)

  override def toString(domains: IndexedSeq[Domain]) = "ext2d(" + domains(0) + ", " + domains(1) + ")"

  def hasSupport(domains: IndexedSeq[Domain], variablePosition: Int, value: Int): Boolean

  override def check(t: Array[Int]) = matrix.check(t)

  override def unshareMatrix() = {
    matrix2d = super.unshareMatrix().asInstanceOf[Matrix2D]
    matrix2d
  }

  override def dataSize = matrix2d.size
}
final class BinaryExtR(scope: Array[Variable], matrix2d: Matrix2D, shared: Boolean) extends BinaryExt(scope, matrix2d, shared) {
  private val residues: Array[Array[Int]] =
    Array(new Array[Int](scope(0).initDomain.toBitVector.nbWords), new Array[Int](scope(1).initDomain.toBitVector.nbWords))

  def hasSupport(domains: IndexedSeq[Domain], variablePosition: Int, index: Int) = {
    val matrixBV: BitVector = matrix2d.getBitVector(variablePosition, index)
    val dom = domains(1 - variablePosition)
    val part = residues(variablePosition)(index)
    BinaryExt.presenceChecks += 1
    (part >= 0 && dom.intersects(matrixBV, part)) || {
      val intersection = dom.intersects(matrixBV)

      if (intersection >= 0) {
        BinaryExt.checks += 1 + intersection;
        residues(variablePosition)(index) = intersection;
        true;
      } else {
        BinaryExt.checks += matrixBV.nbWords;
        false;
      }
    }
  }

}

final class BinaryExtNR(scope: Array[Variable], matrix2d: Matrix2D, shared: Boolean) extends BinaryExt(scope, matrix2d, shared) {
  def hasSupport(domains: IndexedSeq[Domain], variablePosition: Int, index: Int) = {
    val matrixBV: BitVector = matrix2d.getBitVector(variablePosition, index);
    val intersection = domains(1 - variablePosition).intersects(matrixBV)

    if (intersection >= 0) {
      BinaryExt.checks += 1 + intersection;
      true;
    } else {
      BinaryExt.checks += matrixBV.nbWords;
      false;
    }
  }
}

