package concrete
package constraint
import bitvectors.BitVector

trait BoundResidues extends BC with OpsFixPoint {

  val residues: ResidueManager = {
    if (scope.map(v => v.initDomain.last - v.initDomain.head).sum < 10000) {
      new ResidueManagerFast(scope)
    } else {
      new ResidueManagerMap(scope)
    }
  }

  override def revise(ps: ProblemState, modified: BitVector): Outcome = fixPoint(ps)

  def domOps(doms: Array[Domain], position: Int): Domain = {
    doms(position).filterBounds { value =>
      val residue = residues.getResidue(position, value)

      ((residue ne null) && ctp(doms, residue)) ||
        (findSupport(doms, position, value) match {
          case Some(tuple) =>
            assert(check(tuple))
            residues.updateResidue(tuple)
            true

          case None => false
        })
    }
  }

  def findSupport(doms: Array[Domain], position: Int, value: Int): Option[Array[Int]]

}
