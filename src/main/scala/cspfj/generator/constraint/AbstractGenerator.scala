package cspfj.generator.constraint;

import cspfj.constraint.Constraint
import cspfj.generator.FailedGenerationException
import cspfj.problem.{ Variable, Problem, Domain, BooleanDomain }
import cspom.constraint.CSPOMConstraint
import cspom.variable.CSPOMVariable

abstract class AbstractGenerator(val problem: Problem) {
  def cspom2cspfj(variable: CSPOMVariable) = problem.variable(variable.name);

  def addConstraint(constraint: Constraint) = problem.addConstraint(constraint)

  def addVariable(name: String, domain: Domain) = problem.addVariable(name, domain)

  @throws(classOf[FailedGenerationException])
  def generate(constraint: CSPOMConstraint): Boolean
}

object AbstractGenerator {
  @throws(classOf[FailedGenerationException])
  def booleanDomain(variable: Variable) {
    if (variable.dom == null) {
      variable.dom = new BooleanDomain();
    } else if (!(variable.dom.isInstanceOf[BooleanDomain])) {
      throw new FailedGenerationException(variable + " must be boolean");
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
    cartesianS(v0.dom.allValues, v1.dom.allValues, f)

  def domainFrom(v0: Variable, v1: Variable, f: (Int, Int) => Int) =
    makeDomain(cartesian(v0, v1, f))
}