package concrete
package constraint

import bitvectors.BitVector


trait ResiduesRemovals extends Residues with Removals {

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
        val nd = reviseDomain(doms, p)
        if (nd.isEmpty) {
          Contradiction(scope(p))
        } else {
          current = current.updateDom(scope(p), nd)
        }
      }
      p -= 1
    }
    current
  }

  override def revise(ps: ProblemState) = super[Removals].revise(ps)

}
