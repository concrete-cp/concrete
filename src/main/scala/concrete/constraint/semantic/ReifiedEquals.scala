package concrete.constraint.semantic

import scala.IndexedSeq
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

import concrete.Contradiction
import concrete.Domain
import concrete.EMPTY
import concrete.FALSE
import concrete.IntDomain
import concrete.Revised
import concrete.Singleton
import concrete.TRUE
import concrete.UNKNOWNBoolean
import concrete.Variable
import concrete.constraint.Constraint

class ReifiedEquals(val b: Variable, val v: Variable, val c: Int)
  extends Constraint(Array(b, v)) {
  type State = Unit
  def initState = Unit
  def check(t: Array[Int]) = t(0) match {
    case 1 => t(1) == c
    case 0 => t(1) != c
    case _ => sys.error(s"$b must be boolean")
  }

  override def toString(domains: IndexedSeq[Domain], s: State) = s"$b ${domains(0)} = ($v ${domains(1)} == $c)"

  def revise(domains: IndexedSeq[Domain], s: State) = {
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
        if (d.present(c)) {
          Revised(IndexedSeq(TRUE, d.assign(c)), true)
        } else Contradiction

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