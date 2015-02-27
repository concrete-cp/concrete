package concrete.constraint.semantic

import concrete.Domain
import concrete.EMPTY
import concrete.FALSE
import concrete.ProblemState
import concrete.TRUE
import concrete.UNKNOWNBoolean
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator

class SetIn(val r: Variable, val v: Variable, val c: Set[Int])
  extends Constraint(Array(r, v)) with Residues with TupleEnumerator {

  def check(t: Array[Int]) = t(0) match {
    case 1 => c.contains(t(1))
    case 0 => !c.contains(t(1))
    case _ => sys.error(s"${t(0)} must be boolean")
  }

  override def toString(ps: ProblemState) = s"${r.toString(ps)} = (${v.toString(ps)} in $c)"

}