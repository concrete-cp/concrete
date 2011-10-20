package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.constraint.Residues
import cspfj.problem.Variable
import cspfj.constraint.TupleEnumerator

final class LexLeq(scope: Array[Variable]) extends AbstractConstraint(scope)
  with Residues with TupleEnumerator {
  require(scope.size % 2 == 0)
  val half = scope.size / 2

  override def check: Boolean = {
    for (i <- 0 until half) {

      val v0 = value(i);
      val v1 = value(i + half);
      if (v0 < v1) {
        return true;
      }
      if (v0 > v1) {
        return false;
      }
    }
    return true;
  }

  // @Override
  // public float getEvaluation() {
  // return half;
  // }
  //
  // @Override
  // public boolean revise(RevisionHandler revisator, int reviseCount) {
  // // TODO Auto-generated method stub
  // return false;
  // }

}
