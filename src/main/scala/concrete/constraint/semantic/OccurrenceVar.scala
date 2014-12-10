package concrete.constraint.semantic

import concrete.Domain
import concrete.Revised
import concrete.Variable
import concrete.constraint.Constraint

import concrete.Singleton
import concrete.Contradiction

class OccurrenceVar(val result: Variable, val value: Int,
                    val vars: Array[Variable], val offset: Int = 0)
  extends Constraint(result +: vars) {
  type State = Unit
  def initState = Unit
  def check(tuple: Array[Int]) =
    offset + tuple(0) == (1 until arity).count(i => tuple(i) == value)

  def advise(domains: IndexedSeq[Domain], pos: Int): Int = arity

  def revise(domains: IndexedSeq[Domain], s: State) = {
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

    if (result.isEmpty) {
      Contradiction
    } else if (affected == result.last && canBeAffected > 0) {
      Revised(result +: domains.tail.map { d =>
        if (d.size > 1) d.remove(value) else d
      })
    } else if (result.head == affected + canBeAffected) {
      Revised(result +: domains.tail.map { d =>
        if (d.size > 1 && d.present(value)) d.assign(value) else d
      })

    } else {
      Revised(domains.updated(0, result))
    }

  }

  def simpleEvaluation: Int = ???

}