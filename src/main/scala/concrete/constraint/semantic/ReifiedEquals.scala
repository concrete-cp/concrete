package concrete.constraint.semantic

import concrete.Domain
import concrete.EMPTY
import concrete.FALSE
import concrete.ProblemState
import concrete.TRUE
import concrete.UNKNOWNBoolean
import concrete.Variable
import concrete.constraint.Constraint

class ReifiedEquals(val b: Variable, val v: Variable, val c: Int)
  extends Constraint(Array(b, v)) {

  def check(t: Array[Int]) = t(0) match {
    case 1 => t(1) == c
    case 0 => t(1) != c
    case _ => sys.error(s"$b must be boolean")
  }

  override def toString(ps: ProblemState) = s"${b.toString(ps)} = (${v.toString(ps)} == $c)"

  def revise(ps: ProblemState) = {
    val d = ps.dom(v)
    ps.dom(b) match {
      case UNKNOWNBoolean =>
        if (d.present(c)) {
          if (d.size == 1) {
            ps.updateDomNonEmpty(b, TRUE).entail(this)
          } else {
            ps
          }
        } else {
          ps.updateDomNonEmpty(b, FALSE).entail(this)
        }

      case TRUE  => ps.assign(v, c).entail(this)

      case FALSE => ps.remove(v, c).entail(this)

      case EMPTY => throw new AssertionError()
    }

  }

  def advise(ps: ProblemState, pos: Int) = 1
  def simpleEvaluation: Int = 1

}