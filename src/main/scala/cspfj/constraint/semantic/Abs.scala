package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

final class Abs(val result: Variable, val v0: Variable) extends AbstractConstraint(Array(result, v0)) {

  val corresponding1 = result.domain.allValues map { v0.domain.index }
  val corresponding2 = result.domain.allValues map { v => v0.domain.index(-v) }
  val correspondingR = v0.domain.allValues map { v => result.domain.index(math.abs(v)) }

  def check = value(0) == math.abs(value(1))

  private def valid(index: Int, variable: Variable) = index >= 0 && variable.domain.present(index)

  def revise(revisator: RevisionHandler, reviseCount: Int) =
    revise(revisator, result, { i =>
      !valid(corresponding1(i), v0) && !valid(corresponding2(i), v0)
    }) && revise(revisator, v0, { i => !valid(correspondingR(i), result) })

  private def revise(revisator: RevisionHandler, v: Variable, f: (Int => Boolean)): Boolean = {
    var change = false

    for (i <- v.domain if f(i)) {
      v.domain.remove(i)
      change = true
    }

    if (change) {
      if (v.domain.size == 0) {
        return false
      }
      if (v.domain.size == 1) {
        entail()
      }
      revisator.revised(this, v)
    }
    true
  }

  def toString = result + " = |" + v0 + "|";

  def getEvaluation = result.domain.size * 3 / 2 + v0.domain.size
}
