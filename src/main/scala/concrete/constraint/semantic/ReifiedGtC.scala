package concrete.constraint.semantic

import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.TupleEnumerator
import concrete.constraint.Residues
import concrete.Domain
import concrete.ProblemState

class ReifiedGtC(val b: Variable, val v: Variable, val c: Int, val strict: Boolean)
  extends Constraint(Array(b, v))
  with Residues with TupleEnumerator {

  def check(t: Array[Int]) = if (strict) {
    t(0) match {
      case 1 => t(1) > c
      case 0 => t(1) <= c
      case _ => sys.error(s"$b must be boolean")
    }
  } else {
    t(0) match {
      case 1 => t(1) >= c
      case 0 => t(1) < c
      case _ => sys.error(s"$b must be boolean")
    }
  }

  override def toString(ps: ProblemState) =
    s"${b.toString(ps)} = ${v.toString(ps)} ${if (strict) ">" else ">="} $c"

}