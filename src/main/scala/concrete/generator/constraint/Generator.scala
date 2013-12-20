package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.generator.FailedGenerationException
import concrete.{ Variable, Problem, Domain, BooleanDomain }
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import concrete.constraint.extension.ExtensionConstraint
import cspom.CSPOMConstraint
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.IntConstant
import concrete.UndefinedDomain
import concrete.UndefinedDomain
import concrete.IntDomain
import concrete.UndefinedDomain
import concrete.UndefinedDomain
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMFalse
import cspom.variable.BoolVariable

sealed trait C2Conc {
  def is(o: Any): Boolean
}
sealed trait C21D extends C2Conc {
  def values: Seq[Int]
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
  def is(o: Any) = o match {
    case o: Int => o == i
    case _ => false
  }
}
final case class Sequence(s: Seq[C2Conc]) extends C2Conc {

  def is(o: Any) = ???
  //def undefined = s.exists(_.undefined)
}

trait Generator {

  def gen(constraint: CSPOMConstraint)(implicit problem: Problem): Option[Seq[Constraint]] = None

  def genReversed(constraint: CSPOMConstraint)(implicit problem: Problem): Option[Seq[Constraint]] = None

  def genFunctional(constraint: CSPOMConstraint, result: C2Conc)(implicit problem: Problem): Option[Seq[Constraint]] = None

  @throws(classOf[FailedGenerationException])
  final def generate(constraint: CSPOMConstraint, problem: Problem) = {
    constraint.result match {
      case CSPOMTrue => gen(constraint)(problem)
      case CSPOMFalse => genReversed(constraint)(problem)
      case v: CSPOMExpression => genFunctional(constraint, Generator.cspom2concrete(v)(problem))(problem)
    }
  }
}

object Generator {

  def undefinedVar(s: AnyRef*): Seq[Variable] = s.toStream collect {
    case v: Variable => v
    case Var(v) => v
    //case v: Any => throw new IllegalArgumentException(s"$v is not supported")
  } filter {
    _.dom.undefined
  }

  final def cspom2concrete(variable: CSPOMExpression)(implicit problem: Problem): C2Conc = variable match {
    case v: CSPOMVariable => Var(cspomVar2concrete(v))
    case seq: CSPOMSeq[CSPOMExpression] => Sequence(seq.values map cspom2concrete)
    case i: IntConstant => Const(i.value)
    case CSPOMTrue => Const(1)
    case CSPOMFalse => Const(0)
    case _ => throw new UnsupportedOperationException(s"$variable is unexpected")
  }

  final def cspom2concrete1D(variable: CSPOMExpression)(implicit problem: Problem): C21D = cspom2concrete(variable) match {
    case v: Var => v
    case v: Const => v
    case _ => fail(s"Variable or constant expected, $variable found")
  }

  final def cspomVar2concrete(variable: CSPOMVariable)(implicit problem: Problem) = problem.variable(variable.name)

  final def cspom2concreteVar(variable: CSPOMExpression)(implicit problem: Problem) = cspom2concrete(variable) match {
    case Var(v) => v
    case _ => fail(s"Variable expected, $variable found")
  }

  final def cspom2concreteSeq(variable: CSPOMExpression)(implicit problem: Problem) = cspom2concrete(variable) match {
    case Sequence(s) => s
    case _ => fail(s"Sequence expected, $variable found")
  }

  final def cspom2concreteSeqVar(e: CSPOMExpression)(implicit problem: Problem) = cspom2concreteSeq(e) map {
    case Var(v) => v
    case a: C2Conc => fail(s"Variable expected, $a found")
  }

  @throws(classOf[FailedGenerationException])
  final def booleanDomain(variable: Variable): BooleanDomain = {

    variable.dom match {
      case d if d.undefined =>
        val bd = new BooleanDomain()
        variable.dom = bd
        bd
      case bd: BooleanDomain => bd
      case _ => fail(s"$variable  must be boolean");
    }

  }

  final def fail(m: String): Nothing = throw new FailedGenerationException(m)

  /**
   * Sorts and remove duplicates from the given sequence to make eligible domain
   */
  final def makeDomain(seq: Seq[Int]): Seq[Int] =
    if (seq.isEmpty) { Nil }
    else {
      val itr = seq.sorted.reverseIterator
      itr.foldLeft(List(itr.next)) { (l, v) => if (l.head == v) l else v :: l }
    }
  //
  //  def cartesianS[A, B, C](s0: Seq[A], s1: Seq[B], f: (A, B) => C) =
  //    for {
  //      i <- s0
  //      j <- s1
  //    } yield f(i, j)
  //
  //  def cartesian[A](v0: Variable, v1: Variable, f: (Int, Int) => A) =
  //    cartesianS(v0.dom.values.toSeq, v1.dom.values.toSeq, f)

  final def domainFrom(x: Variable, y: Variable, f: (Int, Int) => Int): Seq[Int] = {
    val f2: Seq[Int] => Int = { s =>
      val Seq(i, j) = s
      f(i, j)
    }
    domainFromVar(Seq(x, y), f2)
  }

  final def domainFromVar(source: Seq[Variable], f: (Seq[Int] => Int)): Seq[Int] = {
    domainFromSeq(source.map(_.dom.values.toSeq), f)
  }

  final def domainFrom1D(source: Seq[C21D], f: (Seq[Int] => Int)): Seq[Int] = {
    domainFromSeq(source.map(_.values), f)
  }

  final def domainFromSeq(source: Seq[Seq[Int]], f: (Seq[Int] => Int)): Seq[Int] = {
    makeDomain(cartesian(source).map(f).toSeq)
  }

  final def domainFromFlatVar(source: Seq[Variable], f: (Seq[Int] => Iterable[Int])): Seq[Int] = {
    domainFromFlatSeq(source.map(_.dom.values.toSeq), f)
  }

  final def domainFromFlat1D(source: Seq[C21D], f: (Seq[Int] => Iterable[Int])): Seq[Int] = {
    domainFromFlatSeq(source.map(_.values), f)
  }

  final def domainFromFlatSeq(source: Seq[Seq[Int]], f: (Seq[Int] => Iterable[Int])): Seq[Int] = {
    makeDomain(cartesian(source).flatMap(f).toSeq)
  }

  final def cartesian[A](l: Seq[Seq[A]]): Iterator[Seq[A]] =
    if (l.isEmpty) {
      Iterator(Seq())
    } else {
      l.head.iterator.flatMap(i => cartesian(l.tail).map(i +: _))
    }

  final def restrictDomain(v: Variable, values: Traversable[Int]): Unit = v.dom match {
    case UndefinedDomain => v.dom = IntDomain(makeDomain(values.toSeq): _*)
    case d: Domain => d.filterValues(values.toSet)
  }

  final def restrictDomain(v: Variable, d: Domain): Unit = d match {
    case b: BooleanDomain =>
      if (v.dom.undefined) {
        v.dom = new BooleanDomain()
      }
      v.dom.filterValues(b.values.toSet)
    case d: IntDomain => restrictDomain(v, d.values)
    case _ => throw new UnsupportedOperationException
  }

}