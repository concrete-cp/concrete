package concrete.constraint.semantic

import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.Singleton
import concrete.Contradiction
import concrete.ProblemState
import concrete.Outcome

class OccurrenceVar(val result: Variable, val value: Int,
                    val vars: Array[Variable], val offset: Int = 0)
  extends Constraint(result +: vars) {

  def check(tuple: Array[Int]) =
    offset + tuple(0) == (1 until arity).count(i => tuple(i) == value)

  def advise(ps: ProblemState, pos: Int): Int = arity

  def revise(ps: ProblemState): Outcome = {
    var affected = offset
    var canBeAffected = 0

    for (dom <- ps.domains(vars)) {
      if (dom.present(value)) {
        if (dom.size == 1) {
          affected += 1
        } else {
          canBeAffected += 1
        }
      }
    }

    ps.shaveDom(result, affected, affected + canBeAffected).andThen {
      ps =>
        val result = ps.dom(this.result)
        if (affected == result.last && canBeAffected > 0) {
          ps.updateAll(vars) { d =>
            if (d.size > 1) d.remove(value) else d
          }
        } else if (result.head == affected + canBeAffected) {
          ps.updateAll(vars) { d =>
            if (d.size > 1 && d.present(value)) d.assign(value) else d
          }

        } else {
          ps
        }
    }

  }

  def simpleEvaluation: Int = ???

}