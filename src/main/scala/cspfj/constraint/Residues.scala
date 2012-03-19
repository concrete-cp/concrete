package cspfj.constraint;

import cspfj.problem.Variable;
import scala.annotation.tailrec

trait Residues extends VariablePerVariable {

  val last = new ResidueManagerFast(scope)

  def reviseVariable(position: Int, modified: Seq[Int]): Boolean = {
    val dom = scope(position).dom

    dom.filter { index =>
      val residue = last.getResidue(position, index);
      (residue != null && controlTuplePresence(residue)) || (
        if (findSupport(position, index)) {
          last.updateResidue(tuple.clone)
          true
        } else false)
    }

  }

  def findSupport(variablePosition: Int, index: Int): Boolean

  //def getEvaluation = scope.map(_.dom.size).foldLeft(1.0)(_ * _)

}
