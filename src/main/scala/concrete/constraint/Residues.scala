package concrete.constraint;

import concrete.Variable;
import scala.annotation.tailrec

trait ResidueManager {
  def getResidue(position: Int, index: Int): Array[Int]
  def updateResidue(tuple: Array[Int]): Unit
  def remove(tuple: Array[Int]): Unit
}

trait Residues extends VariablePerVariable {

  val residues: ResidueManager = {
    if (this.scopeSize < 1000) {
      new ResidueManagerFast(scope)
    } else {
      new ResidueManagerMap(scope)
    }

  }

  def reviseVariable(position: Int, modified: List[Int]): Boolean =
    scope(position).dom.filter { index =>
      val residue = residues.getResidue(position, index)

      ((residue ne null) && controlTuplePresence(residue)) ||
        (findSupport(position, index) match {
          case Some(tuple) => {
            residues.updateResidue(tuple)
            true
          }
          case None => false
        })
    }

  def findSupport(variablePosition: Int, index: Int): Option[Array[Int]]

  //def getEvaluation = scope.map(_.dom.size).foldLeft(1.0)(_ * _)

}
