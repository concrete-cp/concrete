package concrete.constraint;

import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import cspom.util.BitVector

trait BoundRemovals[A] extends Constraint with Removals with StatefulConstraint[(Array[Domain], A)]  {

  def revise(problemState: ProblemState, mod: BitVector): Outcome = {
    val state = problemState(this)
    val lastDoms = state._1

    val (doms, boundMod) =
      if (lastDoms eq null) {
        (scope.map(problemState.dom), mod)
      } else {
        val doms = lastDoms.clone

        val boundMod = mod.filter { i =>
          doms(i) = problemState.dom(scope(i))
          lastDoms(i).span != doms(i).span
        }

        (doms, boundMod)

      }

    if (boundMod.isEmpty) {
      problemState.updateState(this, (doms, state._2))
    } else {

      val out = reviseBounds(problemState, boundMod, doms, state._2)

      if (out.domainsOption.exists(_ ne problemState.domains)) {
        val newPS = out.asInstanceOf[ProblemState]
        val newDoms = scope.map(newPS.dom(_))

        val newState = newPS(this)
        newPS.updateState(this, (newDoms, newState._2))

      } else {
        out.updateState(this, (doms, state._2))
      }
    }

  }

  def reviseBounds(problemState: ProblemState, modified: BitVector, doms: Array[Domain], data: A): Outcome

}
