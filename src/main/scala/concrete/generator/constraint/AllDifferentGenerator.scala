package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import concrete.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue

final class AllDifferentGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def gen(constraint: CSPOMConstraint) = {
    val Seq(C2S(solverVariables)) = constraint.arguments map cspom2concrete

    val variables = solverVariables.collect { case C2V(v) => v }

    if (variables.exists(_.dom.undefined)) {
      false
    } else {

      for (constant <- solverVariables.collect { case C2C(i) => i }; v <- variables) {
        v.dom.removeVal(constant)
      }

      addConstraint(new AllDifferent2C(variables: _*));
      addConstraint(new AllDifferentBC(variables: _*));
      true;
    }
  }

}
