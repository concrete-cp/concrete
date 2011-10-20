package cspfj.constraint;
import java.util.Arrays

final class ResidueManagerMap(arity: Int) extends ResidueManager {

  val last = (0 until arity).map { _ => Map[Int, Array[Int]]().withDefaultValue(null) } toArray

  def getResidue(position: Int, index: Int) = last(position)(index)

  def updateResidue(residue: Array[Int]) {
    for (i <- residue.indices) {
      last(i) += residue(i) -> residue
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
