package concrete
package constraint
package linear

final class SumAC(
    constant: Int,
    factors: Array[Int],
    scope: Array[Variable],
    mode: SumMode,
    val skipIntervals: Boolean = true) extends Linear(constant, factors, scope, mode) with Residues with TupleEnumerator with BCCompanion {

  override def advise(ps: ProblemState, event: Event, i: Int): Int = {
    if (skip(ps)) -1 else advise(ps, i)
  }

  override def sizeThreshold: Int = Int.MaxValue

  override def toString: String = toString(s"${mode}AC")

  override def toString(ps: ProblemState): String = toString(ps, s"${mode}AC")

  override def findSupport(doms: Array[Domain], position: Int, value: Int): Option[Array[Int]] =
    super[TupleEnumerator].findSupport(doms, position, value)
}
