package concrete.constraint.semantic

import concrete.constraint.Constraint
import concrete.Variable
import concrete.constraint.Residues
import concrete.constraint.BCCompanion
import concrete.constraint.BC

class ConstProdAC(v0: Variable, v1: Variable, r: Int) extends Constraint(Array(v0, v1)) with Residues
  with BCCompanion {
  def checkValues(t: Array[Int]) = r == t(0) * t(1)
  def simpleEvaluation = 1
  def getEvaluation = scope(0).dom.size + scope(1).dom.size
  def skipIntervals = true
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

class ConstProdBC(v0: Variable, v1: Variable, r: Int) extends Constraint(Array(v0, v1)) with BC {
  def checkValues(t: Array[Int]) = r == t(0) * t(1)
  def simpleEvaluation = 1
  def advise(p: Int) = 2

  def shave() = {
    //val bounds = v0.dom.valueInterval * v1.dom.valueInterval - result.dom.valueInterval
    var mod: List[Int] = Nil

    val v1Interval = v1.dom.valueInterval
    if (!v1Interval.contains(0) && v0.dom.intersectVal(r /: v1Interval)) {
      mod ::= 0
    }
    val v0Interval = v0.dom.valueInterval
    if (!v0Interval.contains(0) && v1.dom.intersectVal(r /: v0Interval)) {
      mod ::= 1
    }
    mod
  }

}