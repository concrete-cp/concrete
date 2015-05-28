package concrete.generator.constraint;

import Generator.cspom2concrete1D
import Generator.cspom2concreteVar
import concrete.constraint.Constraint
import concrete.constraint.semantic.EqAC
import concrete.constraint.semantic.EqBC
import concrete.constraint.semantic.Neq
import concrete.constraint.semantic.ReifiedConstraint
import cspom.CSPOMConstraint
import concrete.BooleanDomain
import concrete.constraint.semantic.SumMode
import concrete.constraint.semantic.SumBC
import concrete.constraint.semantic.Sum
import concrete.constraint.semantic.SumNE
import concrete.constraint.semantic.SumAC
import cspom.UNSATException
import concrete.constraint.semantic.Eq

final object EqGenerator extends Generator {

  def params(constraint: CSPOMConstraint[_]): (Boolean, Int) = {
    require(constraint.getParam("neg").isEmpty, "Neg parameter is deprecated")
    require(constraint.getParam("offset").isEmpty, "Offset parameter is deprecated")
    val neg: Boolean = constraint.getParam[Boolean]("neg").getOrElse(false)

    val offset: Int = constraint.getParam[Int]("offset").getOrElse(0)
    (neg, offset)
  }

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = {
    val Seq(a, b) = constraint.arguments.map(cspom2concrete1D)

    val (neg, offset) = params(constraint)
    val negFactor = if (neg) -1 else 1
    (a, b) match {
      case (Const(a), Const(b)) =>
        if (negFactor * a + offset == b) Seq() else throw new UNSATException("Inconsistent constraint: " + constraint)
      case (Var(a), Var(b)) => Eq(neg, a, offset, b)

      case _                => throw new UnsupportedOperationException(s"$constraint is not supported")
    }

  }

  override def genReversed(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = {
    val Seq(a, b) = constraint.arguments.map(cspom2concrete1D)

    val (neg, offset) = params(constraint)
    require(!neg)
    require(offset == 0)
    (a, b) match {
      case (Const(a), Const(b)) if (a != b) => Seq()
      case (Var(a), Var(b))                 => Seq(new Neq(a, b))

      case _                                => throw new UnsupportedOperationException(s"$constraint is not supported")
    }

  }

  override def genFunctional(funcConstraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap): Seq[Constraint] = {
    val Var(result) = r
    val Seq(a, b) = funcConstraint.arguments map cspom2concrete1D
    val (neg, offset) = params(funcConstraint)
    require(!neg)
    require(offset == 0)
    (a, b) match {
      case (Const(a), Const(b)) =>
        require(result.initDomain == BooleanDomain(a == b), s"$funcConstraint is inconsistent")
        Seq()
      case (Const(a), Var(b)) => Seq(
        new ReifiedConstraint(result,
          new SumBC(a, Array(1), Array(b), SumMode.SumEQ),
          new SumNE(a, Array(1), Array(b))))
      case (Var(a), Const(b)) => Seq(
        new ReifiedConstraint(result,
          new SumBC(b, Array(1), Array(a), SumMode.SumEQ),
          new SumNE(b, Array(1), Array(a))))
      case (Var(a), Var(b)) => Seq(
        new ReifiedConstraint(
          result,
          new EqAC(a, b),
          new Neq(a, b)),
        new ReifiedConstraint(
          result,
          new EqBC(a, b),
          new Neq(a, b)))
    }

  }

}
