package concrete.generator.constraint;

import Generator._
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.semantic.Eq
import concrete.constraint.semantic.Neq
import concrete.constraint.semantic.ReifiedConstraint
import concrete.constraint.semantic.ReifiedEquals
import cspom.CSPOMConstraint

final object EqGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = {
    val Seq(a, b) = constraint.arguments.map(cspom2concrete1D)

    val neg: Boolean = constraint.getParam("neg", classOf[Boolean]).getOrElse(false)
    val negFactor = if (neg) -1 else 1
    val offset: Int = constraint.getParam("offset", classOf[Integer]).map(_.toInt).getOrElse(0)

    (a, b) match {
      case (Const(a), Const(b)) =>
        require(negFactor * a + offset == b)
        Seq()
      case (Var(a), Const(b)) =>
        require(a.dom.values.sameElements(Iterator((b - offset) * negFactor)))
        Seq()
      case (Const(a), Var(b)) =>
        require(b.dom.values.sameElements(Iterator(negFactor * a + offset)))
        Seq()
      case (Var(a), Var(b)) =>
        Seq(new Eq(neg, a, offset, b))
    }

  }

  override def genFunctional(funcConstraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap): Seq[Constraint] = {
    val Var(result) = r
    val Seq(a, b) = funcConstraint.arguments map cspom2concrete1D

    (a, b) match {
      case (Const(a), Const(b)) => ???
      case (Const(a), Var(b)) => Seq(new ReifiedEquals(result, b, a))
      case (Var(a), Const(b)) => Seq(new ReifiedEquals(result, a, b))
      case (Var(a), Var(b)) => Seq(
        new ReifiedConstraint(
          result,
          new Eq(a, b),
          new Neq(a, b)))
    }

  }

}
