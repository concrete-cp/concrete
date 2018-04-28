package concrete
package heuristic
package value


final class RevLexico() extends ValueSelector {


  override def toString: String = "rev-lexico"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def select(ps: ProblemState, variable: Variable, candidates: Domain): (Outcome, Domain) = {
    (ps, Singleton(candidates.last))
  }

  def shouldRestart = false
}
