package cspfj.generator.constraint;

import cspfj.constraint.Constraint
import cspfj.constraint.semantic.Eq
import cspfj.constraint.semantic.Neq
import cspfj.constraint.semantic.ReifiedConstraint
import cspfj.generator.FailedGenerationException
import cspfj.Problem
import cspfj.Variable
import cspom.constraint.CSPOMConstraint
import cspom.constraint.FunctionalConstraint
import cspom.constraint.GeneralConstraint;
import cspfj.constraint.semantic.NeqVec

final class NeqVecGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generateGeneral(constraint: GeneralConstraint) = {
    require(constraint.arity % 2 == 0);

    val scope = constraint.scope map cspom2cspfj

    if (scope exists { _.dom.undefined }) {
      false
    } else {
      val (x, y) = scope.splitAt(scope.length / 2)
      addConstraint(new NeqVec(x.toArray, y.toArray))
      true
    }
  }

}
