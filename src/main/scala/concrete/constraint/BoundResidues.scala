package concrete.constraint;

import concrete.Contradiction
import concrete.Outcome
import concrete.ProblemState
import concrete.Domain

trait BoundResidues extends BC {

  val residues: ResidueManager = {
    if (scope.map(v => v.initDomain.last - v.initDomain.head).sum < 10000) {
      new ResidueManagerFast(scope)
    } else {
      new ResidueManagerMap(scope)
    }

  }

  def reviseDomain(state: ProblemState, position: Int): Domain = {
    state.dom(scope(position)).filterBounds { value =>
      val residue = residues.getResidue(position, value)

      ((residue ne null) && controlTuplePresence(state, residue)) ||
        (findSupport(state, position, value) match {
          case Some(tuple) => {
            assert(check(tuple))
            residues.updateResidue(tuple)
            true
          }
          case None => false
        })
    }
  }

  //@annotation.tailrec
  override def revise(state: ProblemState): Outcome = {
    fixPoint(state, 0 until arity, { (current, p) =>
      current.updateDom(scope(p), reviseDomain(state, p))

      //    if (p < 0) { state }
      //    else if (p == skip) revise(state, skip, p - 1)
      //    else state.updateDom(scope(p), reviseDomain(state, p)).andThen(ps => revise(ps, skip, p - 1))

    })
  }

  def findSupport(state: ProblemState, position: Int, value: Int): Option[Array[Int]]

}
