package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.constraint.semantic.Eq
import concrete.constraint.semantic.Neq
import concrete.constraint.semantic.ReifiedConstraint
import concrete.generator.FailedGenerationException
import concrete.Problem
import concrete.Variable
import cspom.constraint.CSPOMConstraint
import cspom.constraint.FunctionalConstraint
import cspom.constraint.GeneralConstraint;
import concrete.constraint.semantic.NeqVec

final class NeqVecGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generateGeneral(constraint: GeneralConstraint) = {
    require(constraint.arity % 2 == 0);

    val scope = constraint.scope map cspom2concrete

    if (scope exists { _.dom.undefined }) {
      false
    } else {
      val (x, y) = scope.splitAt(scope.length / 2)
      addConstraint(new NeqVec(x.toArray, y.toArray))
      true
    }
  }

}
