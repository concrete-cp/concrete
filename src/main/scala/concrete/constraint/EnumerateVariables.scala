package concrete.constraint

import bitvectors.BitVector
import concrete.{Domain, Outcome, ProblemState}

trait EnumerateVariables extends Constraint {
  def reviseDomain(doms: Array[Domain], position: Int): Domain

  def revise(state: ProblemState, modified: BitVector): Outcome = {
    revise(state, skip(modified)).entailIfFree(this)
  }

  private def revise(state: ProblemState, skip: Int): Outcome = {
    logger.debug(s"skipping $skip")
    val doms = scope.map(state.dom)
    var p = arity - 1
    var current: Outcome = state
    while (p >= 0) {
      if (p != skip) {
        current = current.updateDom(scope(p), reviseDomain(doms, p))
        if (!current.isState) {
          return current
        }
      }
      p -= 1
    }
    current
  }

}
