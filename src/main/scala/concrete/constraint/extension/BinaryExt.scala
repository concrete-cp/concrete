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

import concrete.Domain
import concrete.Variable
import cspom.util.BitVector
import cspom.Statistic
import concrete.constraint.Removals
import concrete.Contradiction
import concrete.ProblemState
import concrete.Outcome

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
    extends ConflictCount(scope, matrix2d, shared) with Removals {

  val idx = scope(0).id
  val idy = scope(1).id

  private val GAIN_OVER_GENERAL = 3;

  private var staticEvaluation: Int = _

  override def getEvaluation(ps: ProblemState) = staticEvaluation

  override def init(ps: ProblemState) = {
    staticEvaluation = (ps.dom(idx).size * ps.dom(idy).size) / GAIN_OVER_GENERAL
    ps
  }

  override def simpleEvaluation = 2

  def revise(ps: ProblemState, mod: List[Int]): Outcome = {
    val skip = this.skip(mod)

    {
      if (skip == 0 || supportCondition(ps, 0)) {
        ps
      } else {
        ps.filterDom(idx)(hasSupport(ps, 0, _))
      }
    }
      .andThen { ps0 =>
        if (skip == 1 || supportCondition(ps0, 1)) {
          ps0
        } else {
          ps0.filterDom(idy)(hasSupport(ps0, 1, _))
        }
      }
      .entailIfFree(this)
  }

  def removeTuple(tuple: Array[Int]) = {
    set(tuple, false)
  }

  def removeTuples(base: Array[Int]) = {
    matrix2d
      .allowed
      .count { tuple =>
        (tuple, base).zipped.forall { (t, b) => b < 0 || t == b } &&
          removeTuple(tuple)
      }

  }

  def hasSupport(ps: ProblemState, variablePosition: Int, value: Int): Boolean

  override def check(t: Array[Int]) = matrix.check(t)

  override def unshareMatrix() = {
    matrix2d = super.unshareMatrix().asInstanceOf[Matrix2D]
    matrix2d
  }

  override def dataSize = matrix2d.size
}

final class BinaryExtR(scope: Array[Variable], matrix2d: Matrix2D, shared: Boolean) extends BinaryExt(scope, matrix2d, shared) {
  private val offsets = Array(scope(0).initDomain.head, scope(1).initDomain.head)

  private val residues: Array[Array[Int]] =
    Array(
      new Array[Int](scope(0).initDomain.last - offsets(0) + 1),
      new Array[Int](scope(1).initDomain.last - offsets(1) + 1))

  def hasSupport(ps: ProblemState, variablePosition: Int, value: Int) = {
    val matrixBV: BitVector = matrix2d.getBitVector(variablePosition, value)
    val otherPosition = 1 - variablePosition
    val otherDom = ps.dom(scope(otherPosition))
    val index = value - offsets(variablePosition)
    val part = residues(variablePosition)(index)
    BinaryExt.presenceChecks += 1
    (part >= 0 && otherDom.toBitVector(matrix2d.offsets(otherPosition)).intersects(matrixBV, part)) || {
      val intersection = otherDom.toBitVector(matrix2d.offsets(otherPosition)).intersects(matrixBV)

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
  def hasSupport(ps: ProblemState, variablePosition: Int, index: Int) = {
    val matrixBV: BitVector = matrix2d.getBitVector(variablePosition, index);
    val otherPosition = 1 - variablePosition
    val intersection = ps.dom(scope(otherPosition)).toBitVector(matrix2d.offsets(otherPosition)).intersects(matrixBV)

    if (intersection >= 0) {
      BinaryExt.checks += 1 + intersection;
      true;
    } else {
      BinaryExt.checks += matrixBV.nbWords;
      false;
    }
  }
}

