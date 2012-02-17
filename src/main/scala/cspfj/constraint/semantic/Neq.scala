package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.problem.Domain
import cspfj.problem.Variable

final class Neq(v0: Variable, v1: Variable) extends AbstractConstraint(Array(v0, v1)) {

  def check = value(0) != value(1)

  def revise() {
    revise(v0, v1)
    revise(v1, v0)
    if (!isEntailed && (v0.dom disjoint v1.dom)) entail()
  }

  private def revise(variable: Variable, otherVar: Variable) = {
    if (variable.dom.size == 1) {
      val index = otherVar.dom.index(variable.dom.firstValue)
      if (index >= 0 && otherVar.dom.present(index)) {
        otherVar.dom.remove(index)
        entail()
      }
    }
  }

  override def toString = v0 + " /= " + v1

  override def getEvaluation =
    if (v0.dom.size == 1 || v1.dom.size == 1) 2
    else (v0.dom.size + v1.dom.size)
}
