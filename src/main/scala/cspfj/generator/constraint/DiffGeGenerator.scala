package cspfj.generator.constraint;

import cspfj.constraint.semantic.{ ReifiedConstraint, Gt }
import cspfj.{ Variable, Problem }
import cspom.constraint.{ GeneralConstraint, FunctionalConstraint, CSPOMConstraint }

final class DiffGeGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generateGeneral(constraint: GeneralConstraint) = {
    require("diffGe" == constraint.description, "Cannot handle" + constraint)
    val Seq(v0, v1, bound) = constraint.scope map cspom2cspfj

    if (bound.dom.size != 1 || Seq(v0, v1, bound).exists(_.dom == null)) {
      false
    } else {
      addConstraint(new Gt(v0, -bound.dom.firstValue, v1, false))
      true
    }
  }

  override def generateFunctional(constraint: FunctionalConstraint) = {
    require("diffGe" == constraint.description, "Cannot handle" + constraint)
    val Seq(result, v0, v1, bound) = constraint.scope map cspom2cspfj

    if (bound.dom.size != 1 || Seq(v0, v1, bound).exists(_.dom == null)) {
      false
    } else {
      AbstractGenerator.booleanDomain(result)
      addConstraint(new ReifiedConstraint(
        result,
        new Gt(v0, -bound.dom.firstValue, v1, false),
        new Gt(v1, bound.dom.firstValue, v0, true)))
      true
    }

  }

}
