package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import concrete.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import cspom.variable.CSPOMConstant
import Generator._
import concrete.constraint.semantic.Neq

final object AllDifferentGenerator extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap) = {
    val arguments = constraint.arguments.map(cspom2concrete(_))

    val vars = arguments.map { case Var(v) => v }
//
//    val constants = arguments.collect { case Const(i) => i }
//
//    require(constants.distinct.size == constants.size, "All-diff with equal constants: " + constraint)
//
//    for (
//      constant <- constants; v <- vars
//    ) {
//      v.dom.removeVal(constant)
//    }

    vars match {
      case Seq() | Seq(_) => Seq()
      case Seq(v0, v1) => Seq(new Neq(v0, v1))
      case v => Seq(new AllDifferent2C(v: _*), new AllDifferentBC(v: _*))
    }

  }

}
