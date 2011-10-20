package cspfj.constraint;

import java.util.Arrays;

import cspfj.problem.Variable;

final class ResidueManagerFast(scope: Array[Variable]) extends ResidueManager {

  val last = scope map (v => new Array[Array[Int]](v.dom.last + 1))

  /*
     * (non-Javadoc)
     * 
     * @see cspfj.constraint.ResidueManager#getResidue(int, int)
     */
  def getResidue(position: Int, index: Int) = last(position)(index)

  /*
     * (non-Javadoc)
     * 
     * @see cspfj.constraint.ResidueManager#updateResidue(int[])
     */
  def updateResidue(residue: Array[Int]) {
    (last, residue).zipped.foreach { (l, r) =>
      l(r) = residue
    }
  }

  def remove(residue: Array[Int]) {
    (last, residue).zipped.foreach { (l, r) =>
      l(r) = null
    }
  }
}
