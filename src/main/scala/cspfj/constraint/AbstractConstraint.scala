package cspfj.constraint
import cspfj.constraint.extension.TupleManager
import cspfj.problem.Variable
import scala.annotation.tailrec

abstract class AbstractConstraint(val scope: Array[Variable]) extends Constraint