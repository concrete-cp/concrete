package concrete.constraint.semantic

import concrete.Contradiction
import concrete.Domain
import concrete.Revised
import concrete.Variable
import concrete.constraint.Constraint

import concrete.Singleton

class OccurrenceConst(val result: Int, val value: Int, val vars: Array[Variable])
  extends Constraint(vars) {

  type State = Unit
  def initState = Unit

  def check(tuple: Array[Int]) =
    result == (0 until arity).count(i => tuple(i) == value)

  def advise(domains: IndexedSeq[Domain], pos: Int): Int = arity

  def revise(domains: IndexedSeq[Domain], s: State) = {
    var affected = 0
    var canBeAffected = 0

    for (d <- domains) {
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
      Revised(domains.map(d => if (d.size > 1) d.remove(value) else d))
    } else if (affected + canBeAffected == result) {
      Revised(domains.map(d => if (d.size > 1 && d.present(value)) d.assign(value) else d))
    } else {
      Revised(domains)
    }

  }

  def simpleEvaluation: Int = ???

}
