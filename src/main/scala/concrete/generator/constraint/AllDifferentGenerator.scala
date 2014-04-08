package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import concrete.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import cspom.variable.CSPOMConstant
import Generator._

final object AllDifferentGenerator extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap) = {
    val arguments = constraint.arguments.map(cspom2concrete(_))

    val vars = arguments.collect { case Var(v) => v }

    if (undefinedVar(vars: _*).nonEmpty) {
      None
    } else {

      val constants = arguments.collect { case Const(i) => i }

      require(constants.distinct.size == constants.size, "All-diff with equal constants: " + constraint)

      if (vars.nonEmpty) {

        for (
          constant <- constants; v <- vars
        ) {
          v.dom.removeVal(constant)
        }

        Some(Seq(new AllDifferent2C(vars: _*), new AllDifferentBC(vars: _*)))
      } else {
        Some(Seq())
      }
    }
  }

}
