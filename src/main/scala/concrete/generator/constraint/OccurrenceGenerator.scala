package concrete.generator.constraint;

import concrete.constraint.semantic.Bounds
import concrete.constraint.semantic.Gcc
import concrete.{ Variable, Problem }
import cspom.{ CSPOMConstraint }
import concrete.constraint.semantic.Occurrence

final class OccurrenceGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def genFunctional(constraint: CSPOMConstraint, r: C2Conc) = {

    val C2V(result) = r
    val args = constraint.arguments map cspom2concreteVar

    if (args.exists(_.dom.undefined) || result.dom.undefined) {
      false
    } else {

      val bound = constraint.params.get("occurence") match {
        case Some(p: Int) => p
        case p: Any => throw new IllegalArgumentException(s"Occurrence constraints requires to be parameterized with an int value, found $p")
      }

      addConstraint(new Occurrence(result, bound, args.toArray));
      true;
    }

  }
}
