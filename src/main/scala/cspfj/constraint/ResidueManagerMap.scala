package cspfj.constraint;
import java.util.Arrays
import scala.annotation.tailrec

final class ResidueManagerMap(arity: Int) {

  val last = (0 until arity).map { _ => Map[Int, Array[Int]]().withDefaultValue(null) } toArray

  def getResidue(position: Int, index: Int) = last(position)(index)

  def updateResidue(residue: Array[Int], i: Int = arity - 1) {
    if (i >= 0) {
      last(i) += residue(i) -> residue
      updateResidue(residue, i - 1)
    }
  }

  def remove(residue: Array[Int]) {
    for (i <- residue.indices) {
      if (Arrays.equals(residue, last(i)(residue(i)))) {
        last(i) -= residue(i)
      }
    }
  }
}
