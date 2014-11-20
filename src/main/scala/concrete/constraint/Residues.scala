package concrete.constraint;

import concrete.Variable
import scala.annotation.tailrec
import concrete.Domain

trait ResidueManager {
  def getResidue(position: Int, index: Int): Array[Int]
  def updateResidue(tuple: Array[Int]): Unit
  def remove(tuple: Array[Int]): Unit
}

trait Residues extends VariablePerVariable {

  val residues: ResidueManager = {
    if (scope.map(_.initDomain.last).sum < 1000) {
      new ResidueManagerFast(scope)
    } else {
      new ResidueManagerMap(scope)
    }

  }

  def reviseVariable(domains: IndexedSeq[Domain], position: Int, modified: List[Int]): Domain =
    domains(position).filter { value =>
      val residue = residues.getResidue(position, value)

      ((residue ne null) && controlTuplePresence(domains, residue)) ||
        (findSupport(domains, position, value) match {
          case Some(tuple) => {
            residues.updateResidue(tuple)
            true
          }
          case None => false
        })
    }

  def findSupport(domains: IndexedSeq[Domain], position: Int, value: Int): Option[Array[Int]]

  //def getEvaluation = scope.map(_.dom.size).foldLeft(1.0)(_ * _)

}
