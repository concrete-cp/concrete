package cspfj.constraint.extension;

import cspfj.constraint.DynamicConstraint;
import cspfj.problem.Variable

trait ExtensionConstraint extends DynamicConstraint {
  def matrixManager: MatrixManager
}

object ExtensionConstraint {

  def newExtensionConstraint(matrix: Matrix, scope: Array[Variable]) =
    matrix match {
      case m: Matrix2D => new ExtensionConstraint2D(scope, m, true)
      case m: TupleSet => new ExtensionConstraintDynamic(scope, m, true)
      case m => new ExtensionConstraintGeneral(m, true, scope)
    }

}