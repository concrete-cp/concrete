package concrete.constraint;

import concrete.Variable
import scala.annotation.tailrec
import concrete.Domain
import concrete.Revised
import concrete.Contradiction
import concrete.ReviseOutcome

trait ResidueManager {
  def getResidue(position: Int, index: Int): Array[Int]
  def updateResidue(tuple: Array[Int]): Unit
  def remove(tuple: Array[Int]): Unit
}

trait Residues extends Removals {

  type State = Unit

  def initState = Unit

  val residues: ResidueManager = {
    if (scope.map(v => v.initDomain.last - v.initDomain.head).sum < 10000) {
      new ResidueManagerFast(scope)
    } else {
      new ResidueManagerMap(scope)
    }

  }

  def reviseDomain(domains: IndexedSeq[Domain], position: Int) = {
    domains(position).filter { value =>
      val residue = residues.getResidue(position, value)

      ((residue ne null) && controlTuplePresence(domains, residue)) ||
        (findSupport(domains, position, value) match {
          case Some(tuple) => {
            assert(check(tuple))
            residues.updateResidue(tuple)
            true
          }
          case None => false
        })
    }
  }

  def revise(domains: IndexedSeq[Domain], modified: List[Int], s: State): ReviseOutcome[Unit] = {
    val s = skip(modified)
    val mod = for (position <- 0 until arity) yield {
      if (position == s) {
        domains(position)
      } else {
        val nd = reviseDomain(domains, position)
        if (nd.isEmpty) return Contradiction
        nd
      }
    }

    Revised(mod, isFree(mod))

  }

  def findSupport(domains: IndexedSeq[Domain], position: Int, value: Int): Option[Array[Int]]

}
