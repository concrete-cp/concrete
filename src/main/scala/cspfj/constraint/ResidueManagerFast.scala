package cspfj.constraint;

import scala.annotation.tailrec
import cspfj.problem.Variable

final class ResidueManagerFast(scope: Array[Variable]) extends ResidueManager {

  val arity = scope.size

  val last = scope map (v => new Array[Array[Int]](v.dom.last + 1))

  def getResidue(position: Int, index: Int) = last(position)(index)

  def updateResidue(residue: Array[Int]) {
    @tailrec
    def upd(i: Int) {
      if (i >= 0) {
        last(i)(residue(i)) = residue
        upd(i - 1)
      }
    }
    upd(arity - 1)
  }

  def remove(residue: Array[Int]) {
    @tailrec
    def rem(i: Int) {
      if (i >= 0) {
        last(i)(residue(i)) = null
        rem(i - 1)
      }
    }
    rem(arity - 1)
  }
}
