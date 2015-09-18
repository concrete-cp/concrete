package concrete.constraint.linear

import concrete.constraint.Residues
import concrete.constraint.BCCompanion
import concrete.Variable
import concrete.ProblemState
import concrete.constraint.TupleEnumerator

final class SumAC(
    constant: Int,
    factors: Array[Int],
    scope: Array[Variable],
    mode: SumMode) extends Linear(constant, factors, scope, mode) with Residues with TupleEnumerator with BCCompanion {

  override def advise(ps: ProblemState, i: Int) = {
    val e = super.advise(ps, i)
    if (skip(ps)) -1 else e
  }

  override def sizeThreshold = Int.MaxValue

  def skipIntervals = true

  override def toString() = toString(s"${mode}AC")

  override def toString(ps: ProblemState) = toString(ps, s"${mode}AC")
}
