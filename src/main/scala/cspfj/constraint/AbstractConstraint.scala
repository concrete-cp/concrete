package cspfj.constraint
import cspfj.constraint.extension.TupleManager
import cspfj.problem.Variable

abstract class AbstractConstraint(_name: String, val scope: Array[Variable]) extends Constraint {

  val name = if (_name == null) "C" + getId else _name
  val arity = scope.size
  val scopeSet = scope.toSet
  val tuple = new Array[Int](arity)
  val tupleManager = new TupleManager(this, tuple)
  val position = scope.zipWithIndex.map { case (value, index) => value -> index }.toMap
  def this(scope: Array[Variable]) = this(null, scope)
  def isInvolved(variable: Variable) = position.contains(variable)
}