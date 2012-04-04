package cspfj.constraint;

import cspfj.problem.Variable;
import scala.annotation.tailrec

trait Residues extends VariablePerVariable {

  val last = new ResidueManagerFast(scope)

  def reviseVariable(position: Int, modified: Seq[Int]): Boolean = {
    val dom = scope(position).dom

    dom.filter { index =>
      val residue = last.getResidue(position, index);
      (residue != null && controlTuplePresence(residue)) ||
        (findSupport(position, index) match {
          case Some(tuple) => {
            last.updateResidue(tuple)
            true
          }
          case None => false
        })
    }

  }

  def findSupport(variablePosition: Int, index: Int): Option[Array[Int]]

  //def getEvaluation = scope.map(_.dom.size).foldLeft(1.0)(_ * _)

}
