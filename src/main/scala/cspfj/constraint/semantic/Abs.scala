package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.filter.RevisionHandler
import cspfj.problem.Variable
import cspfj.constraint.SimpleRemovals

final class Abs(val result: Variable, val v0: Variable) extends AbstractConstraint(Array(result, v0))
  with SimpleRemovals {

  val corresponding1 = result.dom.allValues map { v0.dom.index }
  val corresponding2 = result.dom.allValues map { v => v0.dom.index(-v) }
  val correspondingR = v0.dom.allValues map { v => result.dom.index(math.abs(v)) }

  def check = value(0) == math.abs(value(1))

  private def valid(index: Int, variable: Variable) = index >= 0 && variable.dom.present(index)

  def revise(revisator: RevisionHandler, reviseCount: Int) =
    revise(revisator, result, { i =>
      !valid(corresponding1(i), v0) && !valid(corresponding2(i), v0)
    }) && revise(revisator, v0, { i => !valid(correspondingR(i), result) })

  private def revise(revisator: RevisionHandler, v: Variable, f: (Int => Boolean)): Boolean = {
    var change = false

    for (i <- v.dom.indices if f(i)) {
      v.dom.remove(i)
      change = true
    }

    if (change) {
      if (v.dom.size == 0) {
        return false
      }
      if (v.dom.size == 1) {
        entail()
      }
      revisator.revised(this, v)
    }
    true
  }

  def toString = result + " = |" + v0 + "|";

  def getEvaluation = result.dom.size * 3 / 2 + v0.dom.size
}
