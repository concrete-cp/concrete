package cspfj.constraint
import cspfj.constraint.extension.TupleManager
import cspfj.problem.Variable
import scala.annotation.tailrec

abstract class AbstractConstraint(_name: String, val scope: Array[Variable]) extends Constraint {

  val name = if (_name == null) "C" + getId else _name
  val arity = scope.size
  val scopeSet = scope.toSet
  val tuple = new Array[Int](arity)
  val position = scope.zipWithIndex.toMap
  def this(scope: Array[Variable]) = this(null, scope)
  def isInvolved(variable: Variable) = position.contains(variable)

  @tailrec
  private def sizes(a: Array[Int], i: Int): Array[Int] =
    if (i < 0) a
    else {
      a(i) = scope(i).dom.size
      sizes(a, i - 1)
    }

  def sizes: Array[Int] = {
    // scope map (_.dom.size)
    // Optimize this!
    sizes(new Array[Int](arity), arity - 1)
  }
}