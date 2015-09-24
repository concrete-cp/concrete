package concrete.constraint;

import concrete.Contradiction
import concrete.Outcome
import concrete.ProblemState

trait ResidueManager {
  def getResidue(position: Int, index: Int): Array[Int]
  def updateResidue(tuple: Array[Int]): Unit
  def remove(tuple: Array[Int]): Unit
}

trait Residues extends Removals {

  def init(ps: ProblemState): Outcome = ps

  val residues: ResidueManager = {
    if (scope.map(v => v.initDomain.last - v.initDomain.head).sum < 10000) {
      new ResidueManagerFast(scope)
    } else {
      new ResidueManagerMap(scope)
    }

  }

  def reviseDomain(state: ProblemState, position: Int) = {
    state.dom(scope(position)).filter { value =>
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
  private def revise(state: ProblemState, skip: Int): Outcome = {
    var p = arity - 1
    var current: Outcome = state
    while (p >= 0) {
      if (p != skip) {
        current = current.updateDom(scope(p), reviseDomain(state, p))
        if (current == Contradiction) return Contradiction
      }
      p -= 1
    }
    current
    //    if (p < 0) { state }
    //    else if (p == skip) revise(state, skip, p - 1)
    //    else state.updateDom(scope(p), reviseDomain(state, p)).andThen(ps => revise(ps, skip, p - 1))

  }

  def revise(state: ProblemState, modified: Seq[Int]): Outcome = // {
    revise(state, skip(modified)).entailIfFree(this)
  //    val skip = this.skip(modified)
  //    var cs = state
  //    for (position <- 0 until arity) {
  //      if (position != skip) {
  //        cs = cs.updateDom(scope(position), reviseDomain(cs, position))
  //        //        val nd = reviseDomain(cs, position)
  //        //        if (nd.isEmpty) return Contradiction
  //        //        cs = state.updateDom(position, nd)
  //      }
  //    }
  //
  //    cs.entailIfFree(this)
  //
  //  }

  def findSupport(state: ProblemState, position: Int, value: Int): Option[Array[Int]]

}
