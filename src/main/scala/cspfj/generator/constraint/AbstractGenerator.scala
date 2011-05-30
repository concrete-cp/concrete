package cspfj.generator.constraint;

import cspfj.constraint.Constraint
import cspfj.exception.FailedGenerationException
import cspfj.problem.{Variable, Problem, Domain, BooleanDomain}
import cspom.constraint.CSPOMConstraint
import cspom.variable.CSPOMVariable

abstract class AbstractGenerator(val problem: Problem) {
  def getSolverVariable(variable: CSPOMVariable) = problem.getVariable(variable.name);

  def addConstraint(constraint: Constraint) = problem.addConstraint(constraint)

  def addVariable(name: String, domain: Domain) = problem.addVariable(name, domain)

  @throws(classOf[FailedGenerationException])
  def generate(constraint: CSPOMConstraint): Boolean;

  def handles: Seq[String];
}

object AbstractGenerator {
  @throws(classOf[FailedGenerationException])
  def booleanDomain(variable: Variable) {
    if (variable.getDomain == null) {
      variable.setDomain(new BooleanDomain());
    } else if (!(variable.getDomain.isInstanceOf[BooleanDomain])) {
      throw new FailedGenerationException(variable + " must be boolean");
    }
  }
}