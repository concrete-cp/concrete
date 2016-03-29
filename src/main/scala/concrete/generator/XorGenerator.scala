package concrete.generator;

import Generator.cspom2concrete
import concrete.constraint.semantic.Xor
import cspom.CSPOMConstraint

object XorGenerator extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap) = {
    val vars = constraint.arguments
      .map(cspom2concrete)
      .map(_.asVariable)

    Seq(new Xor(vars.toArray))
  }

}
