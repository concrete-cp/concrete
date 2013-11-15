package concrete.constraint.semantic

import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.TupleEnumerator
import concrete.constraint.Residues

class ReifiedEquals(val b: Variable, val v: Variable, val c: Int)
  extends Constraint(Array(b, v))
  with Residues with TupleEnumerator {

  def checkValues(t: Array[Int]) = t(0) match {
    case 1 => t(1) == c
    case 0 => t(1) != c
    case _ => sys.error(s"$b must be boolean")
  }

  override def toString = s"$b = $v == $c"

}