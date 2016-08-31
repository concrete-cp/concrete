package concrete.generator;

import concrete.BooleanDomain
import concrete.IntDomain
import concrete.Variable
import concrete.constraint.Constraint
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable

sealed trait C2Conc {
  def asVariable(pg: ProblemGenerator): Variable
}
sealed trait C21D extends C2Conc {
  // def values: Seq[Int]
  def is(o: Any): Boolean
}
final case class Var(v: Variable) extends C21D {
  //def values = v.dom.values.toSeq
  def is(o: Any) = o match {
    case o: Variable => o eq v
    case _ => false
  }
  def asVariable(pg: ProblemGenerator) = v
}
final case class Const[T <: AnyVal](i: T) extends C21D {
  //def values = Seq(i)
  def is(o: Any) = o == i
  def asVariable(pg: ProblemGenerator) = i match {
    case i if i == 0 && pg.intToBool => new Variable("false", BooleanDomain(false))
    case i if i == 1 && pg.intToBool => new Variable("true", BooleanDomain(true))
    case i: Int => new Variable(i.toString, IntDomain.ofSeq(i))
    case b: Boolean => new Variable(b.toString, BooleanDomain(b))
    case o => throw new IllegalArgumentException(s"Type of $o is not supported")
  }
}
final case class Sequence(s: Seq[C2Conc], i: Seq[Int]) extends C2Conc {
  def asVariable(pg: ProblemGenerator) = throw new AssertionError
}

trait Generator {

  type VarMap = Map[CSPOMVariable[_], Variable]

  def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] =
    genFunctional(constraint, Generator.cspom2concrete(constraint.result))

  def genReversed(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] =
    genFunctional(constraint, Generator.cspom2concrete(constraint.result))

  def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap): Seq[Constraint] = ???

  final def generate[A](constraint: CSPOMConstraint[A], variables: VarMap): Seq[Constraint] = {
    constraint.result match {
      case CSPOMConstant(true) => gen(constraint.asInstanceOf[CSPOMConstraint[Boolean]])(variables)
      case CSPOMConstant(false) => genReversed(constraint.asInstanceOf[CSPOMConstraint[Boolean]])(variables)
      case v => genFunctional(constraint, Generator.cspom2concrete(v)(variables))(variables)
    }
  }
}

object Generator {
  final def cspom2concrete[A](variable: CSPOMExpression[A])(
    implicit variables: Map[CSPOMVariable[_], Variable]): C2Conc = variable match {
    case v: CSPOMVariable[A] => Var(cspomVar2concrete(v))
    case seq: CSPOMSeq[A] => Sequence(seq.values map cspom2concrete, seq.definedIndices)
    case CSPOMConstant(b: Boolean) => Const(b)
    case CSPOMConstant(i: Int) => Const(i)
    case _ => fail(s"$variable is unexpected")
  }

  final def cspom2concrete1D[A](variable: CSPOMExpression[A])(
    implicit variables: Map[CSPOMVariable[_], Variable]): C21D =
    cspom2concrete(variable) match {
      case v: Var => v
      case v: Const[_] => v
      case _ => fail(s"Variable or constant expected, $variable found")
    }

  final def cspomVar2concrete[A](variable: CSPOMVariable[A])(implicit variables: Map[CSPOMVariable[_], Variable]) =
    variables(variable)

  final def cspom2concreteVar[A](variable: CSPOMExpression[A])(implicit variables: Map[CSPOMVariable[_], Variable]) = cspom2concrete(variable) match {
    case Var(v) => v
    case _ => fail(s"Variable expected, $variable found")
  }

  final def cspom2concreteIndexedSeq[A](variable: CSPOMExpression[A])(implicit variables: Map[CSPOMVariable[_], Variable]): Seq[(Int, C2Conc)] =
    cspom2concrete(variable) match {
      case Sequence(s, i) => i zip s
      case _ => fail(s"Sequence expected, $variable found")
    }

  final def cspom2concreteSeq[A](variable: CSPOMExpression[A])(implicit variables: Map[CSPOMVariable[_], Variable]) = cspom2concrete(variable) match {
    case Sequence(s, _) => s
    case _ => fail(s"Sequence expected, $variable found")
  }

  final def cspom2concreteSeqVar[A](e: CSPOMExpression[A])(implicit variables: Map[CSPOMVariable[_], Variable]) = cspom2concreteSeq(e) map {
    case Var(v) => v
    case a: C2Conc => fail(s"Variable expected, $a found")
  }

  final def fail(m: String): Nothing = throw new FailedGenerationException(m)

}