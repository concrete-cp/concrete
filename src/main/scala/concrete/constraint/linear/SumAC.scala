package concrete
package constraint
package linear

final class SumAC(
    constant: Int,
    factors: Array[Int],
    scope: Array[Variable],
    mode: SumMode) extends Linear(constant, factors, scope, mode) with Residues with TupleEnumerator with BCCompanion {

  override def advise(ps: ProblemState, event: Event, i: Int) = {
    if (skip(ps)) -1 else getEvaluation(ps)
  }

  override def sizeThreshold = Int.MaxValue

  def skipIntervals = true

  override def toString() = toString(s"${mode}AC")

  override def toString(ps: ProblemState) = toString(ps, s"${mode}AC")

  override def findSupport(doms: Array[Domain], position: Int, value: Int): Option[Array[Int]] =
    super[TupleEnumerator].findSupport(doms, position, value)
}
