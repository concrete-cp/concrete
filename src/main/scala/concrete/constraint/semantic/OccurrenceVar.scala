package concrete.constraint.semantic

import concrete.Domain
import concrete.Revised
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Stateless
import concrete.Singleton

class OccurrenceVar(val result: Variable, val value: Int,
                    val vars: Array[Variable], val offset: Int = 0)
  extends Constraint(result +: vars) with Stateless {

  def check(tuple: Array[Int]) =
    offset + tuple(0) == (1 until arity).count(i => tuple(i) == value)

  def advise(domains: IndexedSeq[Domain],pos: Int): Int = arity

  def revise(domains: IndexedSeq[Domain]) = {
    var affected = offset
    var canBeAffected = 0

    for (dom <- domains.view(1, arity)) {
      if (dom.present(value)) {
        if (dom.size == 1) {
          affected += 1
        } else {
          canBeAffected += 1
        }
      }
    }

    val result = domains.head & (affected, affected + canBeAffected)

    if (affected == result.last && canBeAffected > 0) {
      Revised(result +: (1 until arity).map { p =>
        val d = domains(p)
        if (d.size > 1) d.remove(value) else d
      })
    } else if (result.head == affected + canBeAffected) {
      Revised(result +: (1 until arity).map { p =>
        val d = domains(p)
        if (d.size > 1 && d.present(value)) Singleton(value) else d
      })

    } else {
      Revised(domains.updated(0, result))
    }

  }

  def simpleEvaluation: Int = ???

}