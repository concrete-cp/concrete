package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import concrete.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import cspom.variable.CSPOMTrue
import Generator._

final object AllDifferentGenerator extends Generator {
  override def gen(constraint: CSPOMConstraint)(implicit varMap: VarMap) = {
    val variables = constraint.arguments.map(cspom2concreteVar(_))

    if (variables.exists(_.dom.undefined)) {
      None
    } else {

      //      for (constant <- solverVariables.collect { case C2C(i) => i }; v <- variables) {
      //        v.dom.removeVal(constant)
      //      }

      Some(Seq(new AllDifferent2C(variables: _*), new AllDifferentBC(variables: _*)))
    }
  }

}
