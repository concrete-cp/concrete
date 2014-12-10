package concrete.constraint.semantic

import concrete.constraint.Constraint
import concrete.Variable
import concrete.constraint.Residues
import concrete.constraint.BCCompanion

import concrete.Revised
import concrete.Domain
import concrete.constraint.BC

class ConstProdAC(v0: Variable, v1: Variable, r: Int) extends Constraint(Array(v0, v1)) with Residues
  with BCCompanion {
  def check(t: Array[Int]) = r == t(0) * t(1)
  def simpleEvaluation = 2
  def getEvaluation(domain: IndexedSeq[Domain]) = if (skip(domain)) -1 else domain(0).size + domain(1).size
  def skipIntervals = true
  def findSupport(domains: IndexedSeq[Domain], pos: Int, value: Int) = {
    val other = 1 - pos

    if (r % value != 0) {
      None
    } else {
      val sought = r / value

      if (domains(other).present(sought)) {
        val support = new Array[Int](2)
        support(pos) = value
        support(other) = sought
        Some(support)
      } else {
        None
      }

    }
  }
}

class ConstProdBC(v0: Variable, v1: Variable, r: Int) extends Constraint(Array(v0, v1)) with BC {
  type State = Unit
  def initState = Unit
  def check(t: Array[Int]) = r == t(0) * t(1)
  def simpleEvaluation = 1
  def advise(domains: IndexedSeq[Domain], p: Int) = 2

  def shave(domains: IndexedSeq[Domain], s: State) = {
    val v1Interval = domains(1).span
    val v0 = if (v1Interval.contains(0)) domains(0) else domains(0) & (r /: v1Interval)
    val v0Interval = domains(0).span
    val v1 = if (v0Interval.contains(0)) domains(1) else domains(1) & (r /: v0Interval)
    //    //val bounds = v0.dom.valueInterval * v1.dom.valueInterval - result.dom.valueInterval
    //    var mod: List[Int] = Nil
    //
    //    val v1Interval = v1.dom.valueInterval
    //    if (!v1Interval.contains(0) && v0.dom.intersectVal(r /: v1Interval)) {
    //      mod ::= 0
    //    }
    //    val v0Interval = v0.dom.valueInterval
    //    if (!v0Interval.contains(0) && v1.dom.intersectVal(r /: v0Interval)) {
    //      mod ::= 1
    //    }
    Revised(Vector(v0, v1))
  }

}