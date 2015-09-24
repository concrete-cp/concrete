package concrete.generator.constraint;

import Generator.cspom2concrete1D
import concrete.BooleanDomain
import concrete.ParameterManager
import concrete.constraint.linear.LinearLe
import concrete.constraint.linear.Gt
import concrete.constraint.ReifiedConstraint
import concrete.generator.FailedGenerationException
import cspom.CSPOMConstraint

final class GtGenerator(pm: ParameterManager) extends Generator {
  import Generator._
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D;

    val strict = constraint.function match {
      case 'gt => true
      case 'ge => false
      case _   => throw new FailedGenerationException("Unhandled constraint " + constraint);
    }
    (v0, v1) match {
      case (Const(v0: Int), Const(v1: Int)) =>
        require(v0 > v1 || (!strict && v0 >= v1))
        Nil
      case (Var(v0), Const(v1: Int)) =>
        if (strict) {
          require(v0.initDomain.head > v1)
        } else {
          require(v0.initDomain.head >= v1)
        }
        Nil
      case (Const(v0: Int), Var(v1)) =>
        if (strict) {
          require(v0 > v1.initDomain.last)
        } else {
          require(v0 >= v1.initDomain.last)
        }
        Nil
      case (Var(v0), Var(v1)) =>
        Seq(
          new Gt(v0, v1, strict))
      case o => throw new IllegalArgumentException(s"$o contains unsupported domain types")
    }
  }

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val Var(result) = r

    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    val strict = constraint.function match {
      case 'gt => true
      case 'ge => false
      case _   => throw new IllegalArgumentException
    }

    (v0, v1) match {
      case (Const(v0: Int), Const(v1: Int)) =>
        require(result.initDomain == BooleanDomain(v0 > v1 || (!strict && v0 >= v1)), constraint)
        Nil
      case (Var(v0), Const(v1: Int)) =>
        Seq(new ReifiedConstraint(
          result,
          LinearLe(-v1, Array(-1), Array(v0), strict, pm),
          LinearLe(v1, Array(1), Array(v0), !strict, pm)))
      case (Const(v0: Int), Var(v1)) =>
        Seq(new ReifiedConstraint(
          result,
          LinearLe(v0, Array(1), Array(v1), strict, pm),
          LinearLe(-v0, Array(-1), Array(v1), !strict, pm)))

      //Seq(new ReifiedLtC(result, v1, v0, strict))

      case (Var(v0), Var(v1)) => Seq(
        new ReifiedConstraint(
          result,
          new Gt(v0, v1, strict),
          new Gt(v1, v0, !strict)))
      case o => throw new IllegalArgumentException(s"$o contains unsupported domain types")
    }

  }

}
