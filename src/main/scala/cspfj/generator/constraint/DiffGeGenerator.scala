package cspfj.generator.constraint;

import cspfj.constraint.semantic.{ ReifiedConstraint, Gt }
import cspfj.problem.{ Variable, Problem }
import cspom.constraint.{ GeneralConstraint, FunctionalConstraint, CSPOMConstraint }

final class DiffGeGenerator(problem: Problem) extends AbstractGenerator(problem) {

  def generate(constraint: CSPOMConstraint) = {
    require("diffGe" == constraint.description, "Cannot handle" + constraint)
    val generated = constraint match {
      case gC: GeneralConstraint => generateGeneral(gC)
      case fC: FunctionalConstraint => generateReified(fC)
      case _ => throw new IllegalArgumentException(constraint + " not supported");
    }

    if (generated == null) {
      false;
    } else {
      addConstraint(generated);
      true;
    }
  }

  private def generateGeneral(constraint: GeneralConstraint) = {
    val Seq(v0, v1, bound) = constraint.scope map cspom2cspfj

    if (bound.domain.size != 1 || Seq(v0, v1, bound).exists(_.domain == null)) {
      null;
    } else {
      new Gt(v0, -bound.domain.firstValue, v1, false);
    }
  }

  private def generateReified(constraint: FunctionalConstraint) = {
    val Seq(result, v0, v1, bound) = constraint.scope map cspom2cspfj

    if (bound.domain.size != 1 || Seq(v0, v1, bound).exists(_.domain == null)) {
      null;
    } else {
      AbstractGenerator.booleanDomain(result)
      new ReifiedConstraint(
        result,
        new Gt(v0, -bound.domain.firstValue, v1, false),
        new Gt(v1, bound.domain.firstValue, v0, true));
    }

  }

}
