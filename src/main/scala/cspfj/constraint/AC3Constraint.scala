package cspfj.constraint;

import cspfj.problem.Variable;

object AC3Constraint {
  var checks = 0

  def clearStats() { checks = 0 }
}

trait AC3Constraint extends PVRConstraint {

  val last = new ResidueManagerMap(arity)

  def revise(position: Int) = {
    var variable = scope(position)
    var revised = false;
    for (index <- variable.domain) {

      val residue = last.getResidue(position, index);
      if (residue == null || !controlTuplePresence(residue)) {
        if (findSupport(position, index)) {
          last.updateResidue(tuple.clone);
        } else {
          variable.domain.remove(index);
          revised = true;
        }
      }

    }

    revised;
  }

  def findSupport(variablePosition: Int, index: Int): Boolean = {
    tupleManager.setFirstTuple(variablePosition, index);

    do {
      AC3Constraint.checks += 1;
      if (check) {
        return true;
      }
    } while (tupleManager.setNextTuple(variablePosition));

    return false;
  }

  def getEvaluation = scope.map(_.domain.size).foldLeft(1.0)(_ * _)

}
