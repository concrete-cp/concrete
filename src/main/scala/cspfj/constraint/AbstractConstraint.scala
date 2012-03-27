package cspfj.constraint
import cspfj.problem.Variable

abstract class AbstractConstraint(val scope: Array[Variable]) extends Constraint