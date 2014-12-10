package concrete.constraint.semantic

import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.TupleEnumerator
import concrete.constraint.Residues
import concrete.Domain

class ReifiedLtC(val b: Variable, val v: Variable, val c: Int, val strict: Boolean)
  extends Constraint(Array(b, v))
  with Residues with TupleEnumerator {

  def check(t: Array[Int]) = if (strict) {
    t(0) match {
      case 1 => t(1) < c
      case 0 => t(1) >= c
      case _ => sys.error(s"$b must be boolean")
    }
  } else {
    t(0) match {
      case 1 => t(1) <= c
      case 0 => t(1) > c
      case _ => sys.error(s"$b must be boolean")
    }
  }

  override def toString(domains: IndexedSeq[Domain], s: State) =
    s"$b ${domains(0)} = $v ${domains(1)} ${if (strict) "<" else "<="} $c"

}