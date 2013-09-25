package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.constraint.semantic.Eq
import concrete.constraint.semantic.Neq
import concrete.constraint.semantic.ReifiedConstraint
import concrete.generator.FailedGenerationException
import concrete.Problem
import concrete.Variable
import cspom.CSPOMConstraint
import concrete.constraint.semantic.NeqVec

final class NeqVecGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def gen(constraint: CSPOMConstraint) = {
    val Seq(x, y) = constraint.arguments map cspom2concreteSeqVar

    if ((x.iterator ++ y) exists { _.dom.undefined }) {
      false
    } else {
      addConstraint(new NeqVec(x.toArray, y.toArray))
      true
    }
  }

}
