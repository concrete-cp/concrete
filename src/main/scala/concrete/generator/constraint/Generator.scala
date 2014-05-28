package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.generator.FailedGenerationException
import concrete.{ Variable, Problem, Domain, BooleanDomain }
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import concrete.constraint.extension.ExtensionConstraint
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import concrete.UndefinedDomain
import concrete.IntDomain
import cspom.variable.BoolVariable
import scala.reflect.ClassTag
import cspom.variable.CSPOMConstant
import scala.collection.immutable.SortedSet

sealed trait C2Conc
sealed trait C21D extends C2Conc {
  def values: Seq[Int]
  def is(o: Any): Boolean
}
final case class Var(v: Variable) extends C21D {
  def values = v.dom.values.toSeq
  def is(o: Any) = o match {
    case o: Variable => o eq v
    case _ => false
  }
}
final case class Const(i: Int) extends C21D {
  def values = Seq(i)
  def is(o: Any) = o == i
}
final case class Sequence(s: Seq[C2Conc]) extends C2Conc

trait Generator {

  type VarMap = Map[CSPOMVariable[_], Variable]

  def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = ???

  def genReversed(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = ???

  def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap): Seq[Constraint] = ???

  @throws(classOf[FailedGenerationException])
  final def generate[A](constraint: CSPOMConstraint[A], variables: VarMap, problem: Problem) = {
    constraint.result match {
      case CSPOMConstant(true) => gen(constraint.asInstanceOf[CSPOMConstraint[Boolean]])(variables)
      case CSPOMConstant(false) => genReversed(constraint.asInstanceOf[CSPOMConstraint[Boolean]])(variables)
      case v: CSPOMExpression[A] => genFunctional(constraint, Generator.cspom2concrete(v)(variables))(variables)
    }
  }
}

object Generator {
  final def cspom2concrete[A](variable: CSPOMExpression[A])(
    implicit variables: Map[CSPOMVariable[_], Variable]): C2Conc = variable match {
    case v: CSPOMVariable[A] => Var(cspomVar2concrete(v))
    case seq: CSPOMSeq[A] => Sequence(seq.values map cspom2concrete)
    case CSPOMConstant(true) => Const(1)
    case CSPOMConstant(false) => Const(0)
    case CSPOMConstant(i: Int) => Const(i)
    case _ => fail(s"$variable is unexpected")
  }

  final def cspom2concrete1D[A](variable: CSPOMExpression[A])(
    implicit variables: Map[CSPOMVariable[_], Variable]): C21D =
    cspom2concrete(variable) match {
      case v: Var => v
      case v: Const => v
      case _ => fail(s"Variable or constant expected, $variable found")
    }

  final def cspomVar2concrete[A](variable: CSPOMVariable[A])(implicit variables: Map[CSPOMVariable[_], Variable]) =
    variables(variable)

  final def cspom2concreteVar[A](variable: CSPOMExpression[A])(implicit variables: Map[CSPOMVariable[_], Variable]) = cspom2concrete(variable) match {
    case Var(v) => v
    case _ => fail(s"Variable expected, $variable found")
  }

  final def cspom2concreteSeq[A](variable: CSPOMExpression[A])(implicit variables: Map[CSPOMVariable[_], Variable]) = cspom2concrete(variable) match {
    case Sequence(s) => s
    case _ => fail(s"Sequence expected, $variable found")
  }

  final def cspom2concreteSeqVar[A](e: CSPOMExpression[A])(implicit variables: Map[CSPOMVariable[_], Variable]) = cspom2concreteSeq(e) map {
    case Var(v) => v
    case a: C2Conc => fail(s"Variable expected, $a found")
  }

  final def fail(m: String): Nothing = throw new FailedGenerationException(m)

}