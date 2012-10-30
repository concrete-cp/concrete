package cspfj.constraint;

import cspfj.Variable;
import scala.annotation.tailrec

trait Residues extends VariablePerVariable {

  val residues = new ResidueManagerFast(scope)

  def reviseVariable(position: Int, modified: List[Int]): Boolean =
    scope(position).dom.filter { index =>
      val residue = residues.getResidue(position, index);
      
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
