package cspfj.generator.constraint;

import cspfj.constraint.semantic.Bounds
import cspfj.constraint.semantic.Gcc
import cspfj.problem.{ Variable, Problem }
import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }

final class GccGenerator(problem: Problem) extends AbstractGenerator(problem) {
  def generate(constraint: CSPOMConstraint) = {
    require(constraint.isInstanceOf[GeneralConstraint])

    val scope = constraint.scope map cspom2cspfj

    if (scope exists (_.dom == null)) {
      false
    } else {
      val params = constraint.asInstanceOf[GeneralConstraint].predicate.parameters.get.split(", +");

      require(params.size % 3 == 0, "3 parameters per value");

      val bounds = params.grouped(3) map (p => new Bounds(p(0).toInt, p(1).toInt, p(2).toInt))

      addConstraint(new Gcc(scope.toArray, bounds.toArray));
      true;
    }

  }
}