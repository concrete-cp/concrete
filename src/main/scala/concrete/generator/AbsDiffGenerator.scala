package concrete.generator;

import Generator._
import concrete.constraint.Constraint
import concrete.constraint.semantic.AbsDiffAC
import concrete.constraint.semantic.AbsDiffBC
import cspom.CSPOMConstraint
import concrete.constraint.semantic.AbsDiffConstAC
import concrete.constraint.semantic.AbsDiffConstBC

final object AbsDiffGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {
    val result = r.asInstanceOf[C21D]

    val Seq(v0, v1) = constraint.arguments.map(cspom2concreteVar)

    result match {
      case Var(r)        => Seq(new AbsDiffBC(r, v0, v1), new AbsDiffAC(r, v0, v1, true))
      case Const(c: Int) => Seq(new AbsDiffConstBC(c, v0, v1), new AbsDiffConstAC(c, v0, v1))
      case o             => throw new IllegalArgumentException(s"Result $result is not supported")
    }

  }

}
