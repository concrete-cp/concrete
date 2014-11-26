package concrete.constraint;

import concrete.Variable

final class ResidueManagerFast(scope: Array[Variable]) extends ResidueManager {

  val arity = scope.length

  val offsets = scope.map(v => v.initDomain.head)

  val last = Array.tabulate(arity)(i => new Array[Array[Int]](scope(i).initDomain.last - offsets(i) + 1))

  def getResidue(position: Int, value: Int) = last(position)(value - offsets(position))

  def updateResidue(residue: Array[Int]) {
    //println(residue.toSeq)
    var i = arity - 1
    while (i >= 0) {
      last(i)(residue(i) - offsets(i)) = residue
      i -= 1
    }
  }

  def remove(residue: Array[Int]) {
    var i = arity - 1
    while (i >= 0) {
      last(i)(residue(i) - offsets(i)) = null
      i -= 1
    }
  }
}
