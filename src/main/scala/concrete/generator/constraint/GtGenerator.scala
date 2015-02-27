package concrete.generator.constraint;

import Generator.cspom2concrete1D
import concrete.BooleanDomain
import concrete.constraint.Constraint
import concrete.constraint.semantic.Gt
import concrete.constraint.semantic.ReifiedConstraint
import concrete.generator.FailedGenerationException
import cspom.CSPOMConstraint
import concrete.constraint.semantic.SumMode
import concrete.constraint.semantic.SumBC

final object GtGenerator extends Generator {
  import Generator._
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D;

    constraint.function match {
      case 'gt => gte(v0, v1, true)
      case 'ge => gte(v0, v1, false)
      case _   => throw new FailedGenerationException("Unhandled constraint " + constraint);
    }

  }

//  override def genReversed(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
//    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D;
//
//    constraint.function match {
//      case 'gt => gte(v1, v0, false)
//      case 'ge => gte(v1, v0, true)
//      case _   => throw new FailedGenerationException("Unhandled constraint " + constraint);
//    }
//
//  }

  private def gte(v0: C21D, v1: C21D, strict: Boolean): Seq[Constraint] = (v0, v1) match {
    case (Const(v0), Const(v1)) =>
      require(v0 > v1 || (!strict && v0 >= v1))
      Nil
    case (Var(v0), Const(v1)) =>
      if (strict) {
        require(v0.initDomain.head > v1)
      } else {
        require(v0.initDomain.head >= v1)
      }
      Nil
    case (Const(v0), Var(v1)) =>
      if (strict) {
        require(v0 > v1.initDomain.last)
      } else {
        require(v0 >= v1.initDomain.last)
      }
      Nil
    case (Var(v0), Var(v1)) =>
      Seq(
        new Gt(v0, v1, strict))
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
      case (Const(v0), Const(v1)) =>
        val r = v0 > v1 || (!strict && v0 >= v1)
        require(result.initDomain == BooleanDomain(r), constraint)
        Nil
      case (Var(v0), Const(v1)) =>
        Seq(new ReifiedConstraint(
          result,
          new SumBC(-v1, Array(-1), Array(v0), if (strict) SumMode.SumLT else SumMode.SumLE),
          new SumBC(v1, Array(1), Array(v0), if (strict) SumMode.SumLE else SumMode.SumLT)))
      case (Const(v0), Var(v1)) =>
        Seq(new ReifiedConstraint(
          result,
          new SumBC(v0, Array(1), Array(v1), if (strict) SumMode.SumLT else SumMode.SumLE),
          new SumBC(-v0, Array(-1), Array(v1), if (strict) SumMode.SumLE else SumMode.SumLT)))

      //Seq(new ReifiedLtC(result, v1, v0, strict))

      case (Var(v0), Var(v1)) => Seq(
        new ReifiedConstraint(
          result,
          new Gt(v0, v1, strict),
          new Gt(v1, v0, !strict)))
    }

  }

}
