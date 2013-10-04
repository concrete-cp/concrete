package concrete.generator.constraint;

import concrete.constraint.semantic.Bounds
import concrete.constraint.semantic.Gcc
import concrete.{ Variable, Problem }
import cspom.{ CSPOMConstraint }
import concrete.constraint.semantic.OccurrenceVar
import concrete.constraint.semantic.OccurrenceConst

final class OccurrenceGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def genFunctional(constraint: CSPOMConstraint, r: C2Conc) = {

    val args = constraint.arguments map cspom2concreteVar

    if (args.exists(_.dom.undefined) || r.undefined) {
      false
    } else {

      val bound = constraint.params.get("occurrence") match {
        case Some(p: Int) => p
        case p: Any => throw new IllegalArgumentException(s"Occurrence constraints requires to be parameterized with an int value, found $p")
      }

      r match {
        case C2C(result) => addConstraint(new OccurrenceConst(result, bound, args.toArray))
        case C2V(result) => addConstraint(new OccurrenceVar(result, bound, args.toArray))
      }

      true;
    }

  }
}
