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
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator

final object GtGenerator extends Generator {
  import Generator._
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D;

    if (undefinedVar(v0, v1).nonEmpty) {
      None
    } else {
      constraint.function match {
        case 'gt => gte(v0, v1, true)
        case 'ge => gte(v0, v1, false)
        case _ => throw new FailedGenerationException("Unhandled constraint " + constraint);
      }

    }

  }

  override def genReversed(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D;

    if (undefinedVar(v0, v1).nonEmpty) {
      None
    } else {
      constraint.function match {
        case 'gt => gte(v1, v0, false)
        case 'ge => gte(v1, v0, true)
        case _ => throw new FailedGenerationException("Unhandled constraint " + constraint);
      }

    }

  }

  private def gte(v0: C21D, v1: C21D, strict: Boolean): Option[Seq[Constraint]] = (v0, v1) match {
    case (Const(v0), Const(v1)) =>
      require(v0 > v1 || (!strict && v0 >= v1))
      Some(Nil)
    case (Var(v0), Const(v1)) =>
      if (strict) {
        v0.dom.removeToVal(v1)
      } else {
        v0.dom.removeToVal(v1 - 1)
      }
      Some(Nil)
    case (Const(v0), Var(v1)) =>
      if (strict) {
        v1.dom.removeFromVal(v0)
      } else {
        v1.dom.removeFromVal(v0 + 1)
      }
      Some(Nil)
    case (Var(v0), Var(v1)) =>
      Some(Seq(
        new Gt(v0, v1, strict)))
  }

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val Var(result) = r

    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    if (undefinedVar(v0, v1).nonEmpty) {
      None
    } else {

      Generator.booleanDomain(result);

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
          Some(Nil)
        case (Var(v0), Const(v1)) =>
          Some(Seq(new ReifiedGtC(result, v0, v1, strict)))
        case (Const(v0), Var(v1)) =>
          Some(Seq(new ReifiedLtC(result, v1, v0, strict)))

        case (Var(v0), Var(v1)) => Some(Seq(
          new ReifiedConstraint(
            result,
            new Gt(v0, v1, strict),
            new Gt(v1, v0, !strict))))
      }

    }
  }

}
