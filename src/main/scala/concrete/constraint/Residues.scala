package concrete.constraint

import concrete.{Domain, Event, Outcome, ProblemState}

trait Residues extends Constraint with EnumerateVariables {

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




  override def advise(ps: ProblemState, event: Event, pos: Int): Int = advise(ps, pos)

  def advise(ps: ProblemState, pos: Int): Int



}
