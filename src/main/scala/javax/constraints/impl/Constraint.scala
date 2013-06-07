package javax.constraints.impl

import cspom.constraint.CSPOMConstraint

class Constraint(problem: Problem, constraint: CSPOMConstraint) extends AbstractConstraint(problem) {
  setImpl(constraint)
}