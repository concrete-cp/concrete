package cspfj.constraint.extension;

import cspfj.problem.Variable;

final class MatrixManagerGeneral(
  scope: Array[Variable],
  matrix: Matrix,
  shared: Boolean,
  tuple: Array[Int])
  extends AbstractMatrixManager(scope, matrix, shared, tuple);