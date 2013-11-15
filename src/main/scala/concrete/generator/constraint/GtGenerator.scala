package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.constraint.semantic.Gt
import concrete.constraint.semantic.ReifiedConstraint
import concrete.generator.FailedGenerationException
import concrete.Problem
import concrete.Variable
import cspom.CSPOMConstraint
import cspom.variable.CSPOMExpression
import concrete.UNSATObject
import concrete.constraint.semantic.ReifiedGtC
import concrete.constraint.semantic.ReifiedLtC

final class GtGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def gen(constraint: CSPOMConstraint) = {
    require(constraint.arguments.size == 2,
      "Comparison constraints must have exactly two arguments");

    val solverVariables = constraint.arguments map cspom2concrete1D;

    if (solverVariables.collect { case Var(v) => v } exists (_.dom.undefined)) {
      false
    } else {
      constraint.function match {
        case 'gt => gte(solverVariables(0), solverVariables(1), true);
        case 'ge => gte(solverVariables(0), solverVariables(1), false);
        case _ => throw new FailedGenerationException("Unhandled constraint " + constraint);
      }
      true
    }

  }

  private def gte(v0: C21D, v1: C21D, strict: Boolean): Unit = (v0, v1) match {
    case (Const(v0), Const(v1)) => require(v0 > v1 || (!strict && v0 >= v1))
    case (Var(v0), Const(v1)) =>
      if (strict) {
        v0.dom.removeToVal(v1)
      } else {
        v0.dom.removeToVal(v1 - 1)
      }
    case (Const(v0), Var(v1)) =>
      if (strict) {
        v1.dom.removeFromVal(v0)
      } else {
        v1.dom.removeFromVal(v0 + 1)
      }
    case (Var(v0), Var(v1)) =>
      addConstraint(new Gt(v0, v1, strict))
  }

  override def genReified(constraint: CSPOMConstraint, result: Variable) = {

    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    if (v0.undefined || v1.undefined) {
      false
    } else {

      AbstractGenerator.booleanDomain(result);

      val strict = constraint.function match {
        case 'gt => true
        case 'ge => false
        case _ => throw new IllegalArgumentException
      }

      (v0, v1) match {
        case (Const(v0), Const(v1)) =>
          if (v0 > v1 || (!strict && v0 >= v1)) {
            result.dom.setSingle(1)
          } else {
            result.dom.setSingle(0)
          }
        case (Var(v0), Const(v1)) =>
          addConstraint(new ReifiedGtC(result, v0, v1, strict))
        case (Const(v0), Var(v1)) =>
          addConstraint(new ReifiedLtC(result, v1, v0, !strict))

        case (Var(v0), Var(v1)) => addConstraint(new ReifiedConstraint(
          result,
          new Gt(v0, v1, strict),
          new Gt(v1, v0, !strict)));
      }

      true

    }
  }

}
