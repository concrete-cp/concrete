package concrete.constraint.semantic

import concrete.Contradiction
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint

class OccurrenceConst(val result: Int, val value: Int, val vars: Array[Variable])
  extends Constraint(vars) {

  def check(tuple: Array[Int]) =
    result == (0 until arity).count(i => tuple(i) == value)

  def advise(ps: ProblemState, pos: Int): Int = arity

  def revise(ps: ProblemState): Outcome = {
    var affected = 0
    var canBeAffected = 0

    for (d <- ps.domains(vars)) {
      if (d.present(value)) {
        if (d.size == 1) {
          affected += 1
        } else {
          canBeAffected += 1
        }
      }
    }

    if (affected + canBeAffected < result || affected > result) {
      Contradiction
    } else if (affected == result && canBeAffected > 0) {
      ps.updateAll(vars) { d => if (d.size > 1) d.remove(value) else d }
    } else if (affected + canBeAffected == result) {
      ps.updateAll(vars) { d => if (d.size > 1 && d.present(value)) d.assign(value) else d }
    } else {
      ps
    }

  }

  def simpleEvaluation: Int = ???

}
