package concrete.constraint

import bitvectors.BitVector
import concrete.{Domain, Event, Outcome, ProblemState}

trait Residues extends Constraint {

  val residues: ResidueManager = {
    if (scope.map(v => v.initDomain.last - v.initDomain.head).sum < 30000) {
      new ResidueManagerFast(scope)
    } else {
      new ResidueManagerMap(scope)
    }

  }

  def init(ps: ProblemState): Outcome = ps

  def reviseDomain(doms: Array[Domain], position: Int): Domain = {
    doms(position).filter { value =>
      val residue = residues.getResidue(position, value)
      ((residue ne null) && ctp(doms, residue)) || {
        findSupport(doms, position, value) match {
          case Some(tuple) =>
            assert(check(tuple))
            residues.updateResidue(tuple)
            true

          case None =>
            false

        }
      }
    }
  }

  def findSupport(doms: Array[Domain], position: Int, value: Int): Option[Array[Int]]


  def revise(state: ProblemState, modified: BitVector): Outcome = {
    revise(state, skip(modified)).entailIfFree(this)
  }

  override def advise(ps: ProblemState, event: Event, pos: Int): Int = advise(ps, pos)

  def advise(ps: ProblemState, pos: Int): Int



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
