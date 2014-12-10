package concrete.constraint.semantic

import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.TupleEnumerator
import concrete.constraint.Residues
import concrete.TRUE
import concrete.FALSE
import concrete.UNKNOWNBoolean
import concrete.BooleanDomain
import concrete.EMPTY
import concrete.Revised

import concrete.Domain
import concrete.Singleton
import concrete.Contradiction
import concrete.IntDomain
class ReifiedNeq(val b: Variable, val v: Variable, val c: Int)
  extends Constraint(Array(b, v)) {

  type State = Unit
  def initState = Unit

  def check(t: Array[Int]) = t(0) match {
    case 0 => t(1) == c
    case 1 => t(1) != c
    case _ => sys.error(s"$b must be boolean")
  }

  override def toString(domains: IndexedSeq[Domain], s: State) = s"${domains(0)} = ${domains(1)} != $c"

  def revise(domains: IndexedSeq[Domain], s: State) = {
    val d = domains(1)
    domains(0) match {
      case UNKNOWNBoolean =>
        if (d.present(c)) {
          if (d.size == 1) {
            Revised(IndexedSeq(FALSE, d), true)
          } else {
            Revised(domains)
          }
        } else {
          Revised(IndexedSeq(TRUE, d), true)
        }

      case FALSE =>
        if (d.present(c)) {
          Revised(IndexedSeq(TRUE, d.assign(c)), true)
        } else Contradiction

      case TRUE =>
        if (d.present(c)) {
          Revised(IndexedSeq(FALSE, d.remove(c)), true)
        } else {
          Revised(domains, true)
        }

      case EMPTY => throw new AssertionError()
    }

  }

  def advise(domains: IndexedSeq[Domain], pos: Int) = 1
  def simpleEvaluation: Int = 1

}