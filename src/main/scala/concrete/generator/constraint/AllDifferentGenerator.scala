package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import concrete.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import cspom.variable.CSPOMTrue
import Generator._

final object AllDifferentGenerator extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap) = {
    val arguments = constraint.arguments.map(cspom2concrete(_))

    val vars = arguments.collect { case Var(v) => v }

    if (undefinedVar(vars: _*).nonEmpty) {
      None
    } else {

      for (
        constant <- arguments.collect { case Const(i) => i };
        v <- vars
      ) {
        v.dom.removeVal(constant)
      }

      Some(Seq(new AllDifferent2C(vars: _*), new AllDifferentBC(vars: _*)))
    }
  }

}
