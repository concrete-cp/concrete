package cspfj.constraint;

import scala.annotation.tailrec
import cspfj.problem.Variable

final class ResidueManagerFast(scope: Array[Variable]) {

  val arity = scope.size

  val last = scope map (v => new Array[Array[Int]](v.dom.last + 1))

  def getResidue(position: Int, index: Int) = last(position)(index)

  def updateResidue(residue: Array[Int], i: Int = arity - 1) {
    if (i >= 0) {
      last(i)(residue(i)) = residue
      updateResidue(residue, i - 1)
    }
  }

  def remove(residue: Array[Int], i: Int = arity - 1) {

    if (i >= 0) {
      last(i)(residue(i)) = null
      remove(residue, i - 1)
    }

  }
}
