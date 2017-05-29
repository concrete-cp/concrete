package concrete.constraint

import concrete.{Contradiction, Domain, Outcome, ProblemState}

import scala.annotation.tailrec

trait Residues extends Constraint {

  val residues: ResidueManager = {
    if (scope.map(v => v.initDomain.last - v.initDomain.head).sum < 10000) {
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
          case Some(tuple) => {
            assert(check(tuple))
            residues.updateResidue(tuple)
            true
          }
          case None => {
            false
          }
        }
      }
    }
  }

  def findSupport(doms: Array[Domain], position: Int, value: Int): Option[Array[Int]]

  def revise(state: ProblemState): Outcome = {
    val doms = scope.map(state.dom)
    var p = arity - 1
    var current: Outcome = state
    while (p >= 0) {
      val nd = reviseDomain(doms, p)
      if (nd.isEmpty) {
        Contradiction(scope(p))
      } else {
        current = current.updateDom(scope(p), nd)
      }
      p -= 1
    }
    current
  }

  @tailrec
  protected final def ctp(doms: Array[Domain], tuple: Array[Int], i: Int = arity - 1): Boolean = {
    /* Need high optimization */

    i < 0 || (doms(i).present(tuple(i)) && ctp(doms, tuple, i - 1))

  }

}
