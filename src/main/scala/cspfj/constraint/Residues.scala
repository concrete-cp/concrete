package cspfj.constraint;

import cspfj.problem.Variable;

trait Residues extends VariablePerVariable {

  val last = new ResidueManagerMap(arity)

  def revise(position: Int) = {
    var variable = scope(position)
    var revised = false;
    for (index <- variable.dom.indices) {

      val residue = last.getResidue(position, index);
      if (residue == null || !controlTuplePresence(residue)) {
        if (findSupport(position, index)) {
          last.updateResidue(tuple.clone);
        } else {
          variable.dom.remove(index);
          revised = true;
        }
      }

    }

    revised;
  }

  def findSupport(variablePosition: Int, index: Int): Boolean

  //def getEvaluation = scope.map(_.dom.size).foldLeft(1.0)(_ * _)

}
