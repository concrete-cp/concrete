package javax.constraints.impl

import cspom.CSPOMConstraint

class Constraint[+A](problem: Problem, constraint: CSPOMConstraint[A]) extends AbstractConstraint(problem) {
  setImpl(constraint)
}