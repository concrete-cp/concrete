package concrete.constraint.semantic

import concrete.Domain
import concrete.EMPTY
import concrete.FALSE
import concrete.Revised
import concrete.TRUE
import concrete.UNKNOWNBoolean
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Stateless
import concrete.Singleton
import concrete.Contradiction

class ReifiedEquals(val b: Variable, val v: Variable, val c: Int)
  extends Constraint(Array(b, v)) with Stateless {

  def check(t: Array[Int]) = t(0) match {
    case 1 => t(1) == c
    case 0 => t(1) != c
    case _ => sys.error(s"$b must be boolean")
  }

  override def toString = s"$b = $v == $c"

  def revise(domains: IndexedSeq[Domain]) = {
    val d = domains(1)
    domains(0) match {
      case UNKNOWNBoolean =>
        if (d.present(c)) {
          if (d.size == 1) {
            Revised(IndexedSeq(TRUE, d), true)
          } else {
            Revised(domains)
          }
        } else {
          Revised(IndexedSeq(FALSE, d), true)
        }

      case TRUE =>
        if (d.present(c))
          Revised(IndexedSeq(TRUE, Singleton(c)), true)
        else Contradiction

      case FALSE =>
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