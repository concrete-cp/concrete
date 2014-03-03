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
import Generator._

final object NeqVecGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(x, y) = constraint.arguments map cspom2concreteSeqVar

    if (undefinedVar(x.toStream ++ y: _*).nonEmpty) {
      None
    } else {
      Some(Seq(new NeqVec(x.toArray, y.toArray)))
    }
  }

}
