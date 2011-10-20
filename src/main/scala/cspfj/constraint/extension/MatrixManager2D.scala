package cspfj.constraint.extension;

import cspfj.problem.Variable;
import cspfj.util.BitVector;

object MatrixManager2D {
  var checks = 0;
  var presenceChecks = 0

  def clearStats() {
    checks = 0;
    presenceChecks = 0;
  }

};

final class MatrixManager2D(
  scope: Array[Variable],
  private var matrix2d: Matrix2D,
  shared: Boolean,
  tuple: Array[Int]) extends AbstractMatrixManager(scope, matrix2d, shared, tuple) {

  private val MINIMUM_SIZE_FOR_LAST = 3 * java.lang.Long.SIZE;

  /**
   * No need for last data structure if domain sizes <=
   * MINIMUM_SIZE_FOR_LAST
   */
  private val last = if (scope.map(_.dom.maxSize).max > MINIMUM_SIZE_FOR_LAST) {
    Array(new Array[Int](scope(0).dom.maxSize), new Array[Int](scope(1).dom.maxSize))
  } else {
    null
  }

  def hasSupport(variablePosition: Int, index: Int) = {
    if (last == null) {
      hasSupportNR(variablePosition, index);
    } else
      hasSupportR(variablePosition, index);
  }

  private def hasSupportR(variablePosition: Int, index: Int) = {
    controlResidue(variablePosition, index) || {
      val matrixBV = matrix2d.getBitVector(variablePosition, index);
      val intersection = matrixBV
        .intersects(scope(1 - variablePosition).dom.getBitVector);

      if (intersection >= 0) {
        MatrixManager2D.checks += 1 + intersection;
        last(variablePosition)(index) = intersection;
        true;
      } else {
        MatrixManager2D.checks += matrixBV.realSize;
        false;
      }
    }
  }

  private def hasSupportNR(variablePosition: Int, index: Int) = {
    MatrixManager2D.checks += 1;
    matrix2d.getBitVector(variablePosition, index).intersects(
      scope(1 - variablePosition).dom.getBitVector) >= 0;
  }

  private def controlResidue(position: Int, index: Int) = {
    val part = last(position)(index)
    MatrixManager2D.presenceChecks += 1
    (part != -1 &&
      matrix2d.getBitVector(position, index).intersects(
        scope(1 - position).dom.getBitVector, part))
  }

  override def unshareMatrix() = {
    matrix2d = super.unshareMatrix.asInstanceOf[Matrix2D]
    matrix;
  }
}
