package concrete.generator.constraint;

import concrete.constraint.semantic.Bounds
import concrete.constraint.semantic.Gcc
import concrete.{ Variable, Problem }
import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }
import com.sun.org.apache.xerces.internal.impl.xs.models.XSDFACM.Occurence
import cspom.constraint.FunctionalConstraint
import concrete.constraint.semantic.Occurrence

final class OccurrenceGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def generateFunctional(constraint: FunctionalConstraint) = {
    val result = cspom2concrete(constraint.result)
    val args = constraint.arguments map cspom2concrete

    if (args.exists(_.dom.undefined) || result.dom.undefined) {
      false
    } else {

      val bound = constraint.predicate.parameters match {
        case Some(p: Int) => p
        case p: Any => throw new IllegalArgumentException(s"Occurrence constraints requires to be parameterized with an int value, found $p")
      }

      addConstraint(new Occurrence(result, bound, args.toArray));
      true;
    }

  }
}
