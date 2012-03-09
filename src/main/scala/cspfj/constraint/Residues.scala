package cspfj.constraint;

import cspfj.problem.Variable;
import scala.annotation.tailrec

trait Residues extends VariablePerVariable {

  val last = new ResidueManagerFast(scope)

  def reviseVariable(position: Int): Boolean = {
    val dom = scope(position).dom

    dom.filter { index =>
      val residue = last.getResidue(position, index);
      if (residue == null || !controlTuplePresence(residue)) {
        if (findSupport(position, index)) {
          last.updateResidue(tuple.clone)
          true
        } else false
      } else true
    }

  }

  def findSupport(variablePosition: Int, index: Int): Boolean

  //def getEvaluation = scope.map(_.dom.size).foldLeft(1.0)(_ * _)

}
