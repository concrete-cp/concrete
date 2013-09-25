package javax.constraints.impl

import cspom.CSPOMConstraint

class Constraint(problem: Problem, constraint: CSPOMConstraint) extends AbstractConstraint(problem) {
  setImpl(constraint)
}