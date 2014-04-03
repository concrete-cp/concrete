package concrete.generator.constraint;

import concrete.constraint.semantic.Bounds
import concrete.constraint.semantic.Gcc
import concrete.{ Variable, Problem }
import cspom.{ CSPOMConstraint }
import concrete.constraint.semantic.OccurrenceVar
import concrete.constraint.semantic.OccurrenceConst
import Generator._
import cspom.variable.CSPOMConstant

final object OccurrenceGenerator extends Generator {
  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val args = constraint.arguments map cspom2concrete1D

    if (undefinedVar(args: _*).nonEmpty) {
      None
    } else {

      val value: Int = constraint.params.get("occurrence") match {
        case Some(CSPOMConstant(true)) => 1
        case Some(CSPOMConstant(false)) => 0
        case Some(CSPOMConstant(p: Int)) => p
        case p: Any => throw new IllegalArgumentException(s"Occurrence constraints requires to be parameterized with an int value, found $p")
      }

      val scope = args.collect {
        case Var(v) => v
      }

      val constOcc = args.collect {
        case Const(c) => c
      } count {
        _ == value
      }

      r match {
        case Const(result) => Some(Seq(new OccurrenceConst(result - constOcc, value, scope.toArray)))
        case Var(result) =>
          if (result.dom.undefined) {
            Generator.restrictDomain(result, constOcc to (constOcc + scope.count(_.dom.presentVal(value))) iterator)
          }
          Some(Seq(new OccurrenceVar(result, value, scope.toArray, constOcc)))
        case _ => throw new IllegalArgumentException(s"Result must be a variable or constant, was $r")
      }

    }

  }
}
