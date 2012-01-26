package cspfj.constraint;

import cspfj.problem.Variable;

trait Residues extends VariablePerVariable {

  val last = new ResidueManagerMap(arity)

  def reviseVariable(position: Int) = {
    val dom = scope(position).dom
    var revised = false;
    for (index <- dom.indices) {

      val residue = last.getResidue(position, index);
      if (residue == null || !controlTuplePresence(residue)) {
        if (findSupport(position, index)) {
          last.updateResidue(tuple.clone);
        } else {
          dom.remove(index);
          revised = true;
        }
      }

    }

    revised;
  }

  def findSupport(variablePosition: Int, index: Int): Boolean

  //def getEvaluation = scope.map(_.dom.size).foldLeft(1.0)(_ * _)

}
