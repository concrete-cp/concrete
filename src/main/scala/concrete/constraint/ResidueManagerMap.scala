package concrete.constraint;

import concrete.Variable

final class ResidueManagerMap(scope: Array[Variable]) extends ResidueManager {

  val arity = scope.size

  val last = collection.mutable.Map[(Int, Int), Array[Int]]() //scope map (v => new Array[Array[Int]](v.dom.last + 1))

  def getResidue(position: Int, index: Int) = last.getOrElse((position, index), null)

  def updateResidue(residue: Array[Int]) {
    var i = arity - 1
    while (i >= 0) {
      last((i, residue(i))) = residue
      i -= 1
    }
  }

  def remove(residue: Array[Int]) {
    var i = arity - 1
    while (i >= 0) {
      last.remove((i, residue(i)))
      i -= 1
    }
  }
}
