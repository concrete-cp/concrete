package cspfj.constraint;

import cspfj.problem.Variable;
import scala.annotation.tailrec

trait Residues extends VariablePerVariable {

  val last = new ResidueManagerFast(scope)

  def reviseVariable(position: Int) {
    val dom = scope(position).dom

    @tailrec
    def rev(index: Int) {
      if (index >= 0) {

        val residue = last.getResidue(position, index);
        if (residue == null || !controlTuplePresence(residue)) {
          if (findSupport(position, index)) {
            last.updateResidue(tuple.clone)
          } else {
            dom.remove(index)
          }

        }
        rev(dom.next(index))

      }
    }

    rev(dom.first)
  }

  def findSupport(variablePosition: Int, index: Int): Boolean

  //def getEvaluation = scope.map(_.dom.size).foldLeft(1.0)(_ * _)

}
