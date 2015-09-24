package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.constraint.semantic.Neq
import concrete.constraint.ReifiedConstraint
import concrete.generator.FailedGenerationException
import concrete.Problem
import concrete.Variable
import cspom.CSPOMConstraint
import concrete.constraint.semantic.NeqVec
import Generator._

final object NeqVecGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(x, y) = constraint.arguments map cspom2concreteSeqVar

    Seq(new NeqVec(x.toArray, y.toArray))

  }

}
