package concrete.generator.constraint;

import Generator._
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.semantic.AbsDiffAC
import concrete.constraint.semantic.AbsDiffBC
import concrete.constraint.semantic.AbsDiffConst
import cspom.CSPOMConstraint

final object AbsDiffGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {
    val result = r.asInstanceOf[C21D]

    val Seq(v0, v1) = constraint.arguments.map(cspom2concreteVar)

    result match {
      case Var(r) => Seq(new AbsDiffBC(r, v0, v1), new AbsDiffAC(r, v0, v1, true))
      case Const(c) => Seq(new AbsDiffConst(c, v0, v1))
    }

  }

}
