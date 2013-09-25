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

sealed trait C2Conc
sealed trait C21D extends C2Conc {
  def values: Seq[Int]
}
final case class C2V(v: Variable) extends C21D {
  def values = v.dom.values.toSeq
}
final case class C2C(i: Int) extends C21D {
  def values = Seq(i)
}
final case class C2S(s: Seq[C2Conc]) extends C2Conc

abstract class AbstractGenerator(val problem: Problem) {
  def cspom2concrete(variable: CSPOMExpression): C2Conc = variable match {
    case v: CSPOMVariable => C2V(cspomVar2concrete(v))
    case seq: CSPOMSeq => C2S(seq.values map cspom2concrete)
    case i: IntConstant => C2C(i.value)
    case _ => throw new UnsupportedOperationException(s"$variable is unexpected")
  }

  def cspomVar2concrete(variable: CSPOMVariable) = problem.variable(variable.name)

  def cspom2concreteVar(variable: CSPOMExpression) = cspom2concrete(variable) match {
    case C2V(v) => v
    case _ => AbstractGenerator.fail(s"Variable expected, $variable found")
  }

  def cspom2concreteSeq(variable: CSPOMExpression) = cspom2concrete(variable) match {
    case C2S(s) => s
    case _ => AbstractGenerator.fail(s"Sequence expected, $variable found")
  }

  def cspom2concreteSeqVar(e: CSPOMExpression) = cspom2concreteSeq(e) map {
    case C2V(v) => v
    case a: C2Conc => AbstractGenerator.fail(s"Variable expected, $a found")
  }

  def addConstraint(constraint: Constraint) = problem.addConstraint(constraint)

  @throws(classOf[FailedGenerationException])
  final def generate(constraint: CSPOMConstraint) = {
    constraint.result match {
      case CSPOMTrue => gen(constraint)
      case CSPOMFalse => genRev(constraint)
      case b: BoolVariable => genReified(constraint, cspomVar2concrete(b))
      case v: CSPOMExpression => genFunctional(constraint, cspom2concrete(v))
    }
  }

  def gen(constraint: CSPOMConstraint) = false
  def genRev(constraint: CSPOMConstraint) = false
  def genReified(constraint: CSPOMConstraint, result: Variable) = false
  def genFunctional(constraint: CSPOMConstraint, result: C2Conc) = false
}

object AbstractGenerator {
  @throws(classOf[FailedGenerationException])
  def booleanDomain(variable: Variable): BooleanDomain = {
    if (variable.dom.undefined) {
      val bd = new BooleanDomain()
      variable.dom = bd
      bd
    } else {
      variable.dom match {
        case bd: BooleanDomain => bd
        case _ => fail(s"$variable  must be boolean");
      }
    }
  }

  def fail(m: String): Nothing = throw new FailedGenerationException(m)

  /**
   * Sorts and remove duplicates from the given sequence to make eligible domain
   */
  def makeDomain(seq: Seq[Int]) =
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

  def domainFromVar(source: Seq[Variable], f: (Seq[Int] => Int)): List[Int] = {
    val sourceDomains = source.map(_.dom.values.toSeq)
    makeDomain(cartesian(sourceDomains).map(f).toSeq)
  }

  def domainFromVar(x: Variable, y: Variable, f: (Int, Int) => Int): List[Int] =
    domainFromVar(Seq(x, y), { case Seq(i, j) => f(i, j) })

  def domainFromVarFlat(source: Seq[Variable], f: (Seq[Int] => Iterable[Int])) = {
    val sourceDomains = source.map(_.dom.values.toSeq)
    makeDomain(cartesian(sourceDomains).flatMap(f).toSeq)
  }

  def domainFrom(source: Seq[C21D], f: (Seq[Int] => Int)) = {
    val sourceDomains = source.map(_.values)
    makeDomain(cartesian(sourceDomains).map(f).toSeq)
  }

  def domainFromFlat(source: Seq[C21D], f: (Seq[Int] => Iterable[Int])) = {
    val sourceDomains = source.map(_.values)
    makeDomain(cartesian(sourceDomains).flatMap(f).toSeq)
  }

  def cartesian[A](l: Seq[Seq[A]]): Iterator[Seq[A]] =
    if (l.isEmpty) {
      Iterator(Seq())
    } else {
      l.head.iterator.flatMap(i => cartesian(l.tail).map(i +: _))
    }

  def restrictDomain(v: Variable, values: Traversable[Int]): Unit = v.dom match {
    case UndefinedDomain => v.dom = IntDomain(makeDomain(values.toSeq): _*)
    case d: IntDomain => v.dom = IntDomain(makeDomain((d.values.toSet & values.toSet).toSeq): _*)
    case _ => throw new UnsupportedOperationException
  }

}