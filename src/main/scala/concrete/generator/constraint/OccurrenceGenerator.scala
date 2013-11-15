package concrete.generator.constraint;

import concrete.constraint.semantic.Bounds
import concrete.constraint.semantic.Gcc
import concrete.{ Variable, Problem }
import cspom.{ CSPOMConstraint }
import concrete.constraint.semantic.OccurrenceVar
import concrete.constraint.semantic.OccurrenceConst
import cspom.variable.IntConstant

import cspom.variable.CSPOMFalse
import cspom.variable.CSPOMTrue

final class OccurrenceGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def genFunctional(constraint: CSPOMConstraint, r: C2Conc) = {

    val args = constraint.arguments map cspom2concreteVar

    if (args.exists(_.dom.undefined)) {
      false
    } else {

      val value: Int = constraint.params.get("occurrence") match {
        case Some(CSPOMTrue) => 1
        case Some(CSPOMFalse) => 0
        case Some(p: IntConstant) => p.value
        case p: Any => throw new IllegalArgumentException(s"Occurrence constraints requires to be parameterized with an int value, found $p")
      }

      r match {
        case Const(result) => addConstraint(new OccurrenceConst(result, value, args.toArray))
        case Var(result) =>
          if (result.dom.undefined) {
            AbstractGenerator.restrictDomain(result, 0 to args.count(_.dom.present(value)))
          }
          addConstraint(new OccurrenceVar(result, value, args.toArray))
        case _ => throw new IllegalArgumentException(s"Result must be a variable or constant, was $r")
      }

      true;
    }

  }
}
