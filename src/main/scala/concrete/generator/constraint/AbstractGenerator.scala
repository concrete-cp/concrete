package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.generator.FailedGenerationException
import concrete.{ Variable, Problem, Domain, BooleanDomain }
import cspom.constraint.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.constraint.FunctionalConstraint
import cspom.constraint.GeneralConstraint
import cspom.extension.ExtensionConstraint

abstract class AbstractGenerator(val problem: Problem) {
  def cspom2concrete(variable: CSPOMVariable) = problem.variable(variable.name);

  def addConstraint(constraint: Constraint) = problem.addConstraint(constraint)

  @throws(classOf[FailedGenerationException])
  def generate(constraint: CSPOMConstraint) = constraint match {
    case c: GeneralConstraint => generateGeneral(c)
    case c: FunctionalConstraint => generateFunctional(c)
    case c: ExtensionConstraint => generateExtension(c)
  }

  def generateGeneral(constraint: GeneralConstraint) = false

  def generateFunctional(constraint: FunctionalConstraint) = false

  def generateExtension(constraint: ExtensionConstraint) = false
}

object AbstractGenerator {
  @throws(classOf[FailedGenerationException])
  def booleanDomain(variable: Variable) {
    if (variable.dom.undefined) {
      variable.dom = new BooleanDomain();
    } else if (!(variable.dom.isInstanceOf[BooleanDomain])) {
      throw new FailedGenerationException(s"$variable  must be boolean");
    }
  }

  /**
   * Sorts and remove duplicates from the given sequence to make eligible domain
   */
  def makeDomain(seq: Seq[Int]) =
    if (seq.isEmpty) { Nil }
    else {
      val itr = seq.sorted.reverseIterator
      itr.foldLeft(List(itr.next)) { (l, v) => if (l.head == v) l else v :: l }
    }

  def cartesianS[A, B, C](s0: Seq[A], s1: Seq[B], f: (A, B) => C) =
    for {
      i <- s0
      j <- s1
    } yield f(i, j)

  def cartesian[A](v0: Variable, v1: Variable, f: (Int, Int) => A) =
    cartesianS(v0.dom.values.toSeq, v1.dom.values.toSeq, f)

  def domainFrom(v0: Variable, v1: Variable, f: (Int, Int) => Int) =
    makeDomain(cartesian(v0, v1, f))
}