package concrete.constraint.semantic

import concrete.constraint.Constraint
import concrete.Variable
import concrete.constraint.Residues

class ConstProd(v0: Variable, v1: Variable, r: Int) extends Constraint(Array(v0, v1)) with Residues {
  def checkValues(t: Array[Int]) = r == t(0) * t(1)
  def simpleEvaluation = 1
  def getEvaluation = scope(0).dom.size + scope(1).dom.size
  def findSupport(pos: Int, idx: Int) = {
    val other = 1 - pos
    val value = scope(pos).dom.value(idx)
    if (r % value != 0) {
      None
    } else {
      val sought = r / value

      if (scope(other).dom.presentVal(sought)) {
        val support = new Array[Int](2)
        support(pos) = idx
        support(other) = scope(other).dom.index(sought)
        Some(support)
      } else {
        None
      }

    }
  }
}