package concrete.constraint.semantic

import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator

class Occurrence(val result: Variable, val value: Int, val vars: Array[Variable])
  extends Constraint(result +: vars)
  with Residues
  with TupleEnumerator {

  def checkValues(tuple: Array[Int]) =
    tuple(0) == (1 until arity).count(i => tuple(i) == value)

}