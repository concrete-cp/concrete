package concrete.generator.constraint;

import concrete.constraint.semantic.Bounds
import concrete.constraint.semantic.Gcc
import concrete.{ Variable, Problem }
import cspom.{ CSPOMConstraint }
import concrete.constraint.semantic.Occurrence

final class OccurrenceGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def genFunctional(constraint: CSPOMConstraint, r: C2Conc) = {

    val C2V(result) = r
    val Seq(vars) = constraint.arguments map cspom2concreteSeq

    val args = vars map {
      case C2V(v) => v
      case _ => AbstractGenerator.fail("Variable expected")
    }

    if (args.exists(_.dom.undefined) || result.dom.undefined) {
      false
    } else {

      val bound = constraint.params.get("occurrence") match {
        case Some(p: Int) => p
        case p: Any => throw new IllegalArgumentException(s"Occurrence constraints requires to be parameterized with an int value, found $p")
      }

      addConstraint(new Occurrence(result, bound, args.toArray));
      true;
    }

  }
}
