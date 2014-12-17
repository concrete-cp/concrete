package concrete.generator.constraint;

import Generator.cspom2concrete1D
import Generator.cspom2concreteVar
import concrete.constraint.Constraint
import concrete.constraint.semantic.EqAC
import concrete.constraint.semantic.EqBC
import concrete.constraint.semantic.Neq
import concrete.constraint.semantic.ReifiedConstraint
import concrete.constraint.semantic.ReifiedEquals
import cspom.CSPOMConstraint
import concrete.BooleanDomain

final object EqGenerator extends Generator {

  def params(constraint: CSPOMConstraint[_]): (Boolean, Int) = {
    val neg: Boolean = constraint.getParam("neg").getOrElse(false)

    val offset: Int = constraint.getParam("offset").getOrElse(0)
    (neg, offset)
  }

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = {
    val Seq(a, b) = constraint.arguments.map(cspom2concrete1D)

    val (neg, offset) = params(constraint)
    val negFactor = if (neg) -1 else 1
    (a, b) match {
      case (Const(a), Const(b)) =>
        if (negFactor * a + offset == b) Seq() else throw concrete.UNSATObject
      case (Var(a), Var(b)) =>
        Seq(new EqBC(neg, a, offset, b), new EqAC(neg, a, offset, b))

      case _ => throw new UnsupportedOperationException(s"$constraint is not supported")
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
        require(result.initDomain == BooleanDomain(a == b))
        Seq()
      case (Const(a), Var(b)) => Seq(new ReifiedEquals(result, b, a))
      case (Var(a), Const(b)) => Seq(new ReifiedEquals(result, a, b))
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
