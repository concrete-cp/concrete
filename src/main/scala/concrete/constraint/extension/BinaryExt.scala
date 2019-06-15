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

;

import java.util

import bitvectors.BitVector
import concrete._
import cspom.Statistic

object BinaryExt {
  val MINIMUM_SIZE_FOR_LAST: Int = 3 * java.lang.Long.SIZE
  val GAIN_OVER_GENERAL: Int = 3
  @Statistic
  var checks: Long = 0L

  /**
    * No need for residues if domain sizes <= MINIMUM_SIZE_FOR_LAST
    */
  def apply(scope: Array[Variable], matrix2d: Matrix2D): BinaryExt = {
    if (scope.map(_.initDomain.size).max > MINIMUM_SIZE_FOR_LAST) {
      new BinaryExtR(scope, matrix2d)
    } else {
      new BinaryExtNR(scope, matrix2d)
    }
  }
}

abstract class BinaryExt(scope: Array[Variable], val matrix: Matrix2D)
  extends ExtensionConstraint(scope) with ConflictCount {

  protected val x = scope(0)
  protected val y = scope(1)

  private var staticEvaluation: Int = _

  override def advise(ps: ProblemState, event: Event, pos: Int): Int = staticEvaluation

  override def init(ps: ProblemState): Outcome = {
    staticEvaluation = (ps.card(x) * ps.card(y)) / BinaryExt.GAIN_OVER_GENERAL
    ps
  }

  override def simpleEvaluation = 2

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val skip = this.skip(mod)

    val doms = Array(ps.dom(x), ps.dom(y))
    var cs = ps
    if (skip != 0 && !supportCondition(doms, 0)) {
      val otherBV = doms(1).toBitVector(matrix.offsets(1))
      val nd = doms(0).filter(hasSupport(0, _, otherBV))
      if (nd.isEmpty) return Contradiction(Seq(scope(0)))
      if (nd ne doms(0)) {
        doms(0) = nd
        cs = cs.updateDomNonEmptyNoCheck(x, nd)
      }
    }

    if (skip != 1 && !supportCondition(doms, 1)) {
      val otherBV = doms(0).toBitVector(matrix.offsets(0))
      val nd = doms(1).filter(hasSupport(1, _, otherBV))
      if (nd.isEmpty) return Contradiction(Seq(scope(1)))
      if (nd ne doms(1)) {
        doms(1) = nd
        cs = cs.updateDomNonEmptyNoCheck(y, nd)
      }
    }

    if (doms(0).isAssigned || doms(1).isAssigned) {
      cs.entail(this)
    } else {
      cs
    }
  }

  def removeTuples(base: Array[Int]): Int = {
    matrix
      .allowed
      .count { tuple =>
        (tuple lazyZip base).forall { (t, b) => b < 0 || t == b } &&
          removeTuple(tuple)
      }

  }

  def removeTuple(tuple: Array[Int]): Boolean = {
    set(tuple, status = false)
  }

  def hasSupport(variablePosition: Int, value: Int, otherBV: BitVector): Boolean

  override def check(t: Array[Int]): Boolean = matrix.check(t)

  //  override def unshareMatrix() = {
  //    matrix = super.unshareMatrix().asInstanceOf[Matrix2D]
  //    matrix
  //  }

  override def dataSize: Int = matrix.size
}

final class BinaryExtR(scope: Array[Variable], matrix2d: Matrix2D) extends BinaryExt(scope, matrix2d) {
  //private val offsets = Array(x.initDomain.head, y.initDomain.head)

  private val residues: util.HashMap[(Int, Int), Int] = new util.HashMap()


  //  Array[Array[Int]] =
  //    Array(
  //      new Array[Int](scope(0).initDomain.last - offsets(0) + 1),
  //      new Array[Int](scope(1).initDomain.last - offsets(1) + 1))

  def hasSupport(variablePosition: Int, value: Int, otherBV: BitVector): Boolean = {
    val matrixBV: BitVector = matrix2d.getBitVector(variablePosition, value)

    //val index = value - offsets(variablePosition)
    val part = residues.get((variablePosition, value))

    BinaryExt.checks += 1

    otherBV.intersects(matrixBV, part) || {
      val intersection = otherBV.intersects(matrixBV)

      if (intersection >= 0) {
        BinaryExt.checks += 1 + intersection
        residues.put((variablePosition, value), intersection)
        true
      } else {
        BinaryExt.checks += matrixBV.nbWords
        false
      }
    }
  }

}

final class BinaryExtNR(scope: Array[Variable], matrix2d: Matrix2D) extends BinaryExt(scope, matrix2d) {
  def hasSupport(variablePosition: Int, index: Int, otherBV: BitVector): Boolean = {

    val matrixBV: BitVector = matrix2d.getBitVector(variablePosition, index)
    val intersection = otherBV.intersects(matrixBV)
    //println(s"intersection of $otherBV with $matrixBV")
    //println(s"support for $variablePosition, $index: $intersection")
    if (intersection >= 0) {
      BinaryExt.checks += 1 + intersection
      true
    } else {
      BinaryExt.checks += matrixBV.nbWords
      false
    }
  }
}

