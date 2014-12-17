package concrete.constraint.semantic

import concrete.constraint.Constraint
import concrete.Variable
import concrete.constraint.Residues
import concrete.constraint.BCCompanion
import concrete.Domain
import concrete.constraint.BC
import concrete.ProblemState

class ConstProdAC(v0: Variable, v1: Variable, r: Int) extends Constraint(Array(v0, v1)) with Residues
  with BCCompanion {
  def check(t: Array[Int]) = r == t(0) * t(1)
  def simpleEvaluation = 2
  def getEvaluation(ps: ProblemState) = if (skip(ps)) -1 else ps.dom(v0).size + ps.dom(v1).size
  def skipIntervals = true
  def findSupport(ps: ProblemState, pos: Int, value: Int) = {

    if (r % value != 0) {
      None
    } else {
      val sought = r / value
      val other = 1 - pos

      if (ps.dom(scope(other)).present(sought)) {
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
  def advise(ps: ProblemState, p: Int) = 2

  def shave(ps: ProblemState) = {
    val v1Interval = ps.span(v1)
    if (v1Interval.contains(0)) ps else ps.shaveDom(v0, r /: v1Interval)
      .andThen { ps =>
        val v0Interval = ps.span(v0)
        if (v0Interval.contains(0)) ps else ps.shaveDom(v1, r /: v0Interval)
      }

  }

}