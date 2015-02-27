package concrete.constraint.semantic

import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.Singleton
import concrete.Contradiction
import concrete.ProblemState
import concrete.Outcome
import concrete.constraint.StatefulConstraint
import concrete.constraint.Removals
import concrete.util.BitVector

class OccurrenceVar(val result: Variable, val value: Int,
                    val vars: Array[Variable], val offset: Int = 0)
  extends Constraint(result +: vars) with StatefulConstraint with Removals {

  type State = (Int, BitVector)

  def initState = (offset, BitVector.filled(vars.length))

  def check(tuple: Array[Int]) =
    offset + tuple(0) == (1 until arity).count(i => tuple(i) == value)

  def getEvaluation(ps: ProblemState): Int = state(ps)._2.cardinality

  def revise(ps: ProblemState, mod: List[Int]): Outcome = {

    val oldState = state(ps)

    var affected = oldState._1
    var canBeAffectedSet = oldState._2

    for (m <- mod) {
      if (m > 0 && canBeAffectedSet(m - 1)) {
        val dom = ps.dom(scope(m))
        if (!dom.present(value)) {
          canBeAffectedSet -= m - 1
        } else if (dom.size == 1) {
          canBeAffectedSet -= m - 1
          affected += 1
        }
      }
    }

    val canBeAffected = canBeAffectedSet.cardinality

    ps.shaveDom(result, affected, affected + canBeAffected).andThen {
      ps =>
        val result = ps.dom(this.result)
        if (affected == result.last && canBeAffected > 0) {
          var state = ps
          for (p <- canBeAffectedSet.iterator) {
            val v = vars(p)
            state = state.updateDomNonEmpty(v, state.dom(v).remove(value))
          }
          //canBeAffectedSet = BitVector.empty

          state.entail(this)
          //          ps.updateAll(vars) { d =>
          //            if (d.size > 1) d.remove(value) else d
          //          }
        } else if (result.head == affected + canBeAffected) {
          //affected = result.head
          var state = ps
          for (p <- canBeAffectedSet.iterator) {
            val v = vars(p)
            state = state.updateDomNonEmpty(v, state.dom(v).assign(value))
          }
          //canBeAffectedSet = BitVector.empty
          state.entail(this)
          //          ps.updateAll(vars) { d =>
          //            if (d.size > 1 && d.present(value)) d.assign(value) else d
          //          }

        } else {
          ps.updateState(this, (affected, canBeAffectedSet))
        }

    }

  }

  def simpleEvaluation: Int = ???

}