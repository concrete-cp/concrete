package cspfj.constraint.semantic;

import cspfj.constraint.Constraint
import cspfj.problem.Domain
import cspfj.problem.Variable

final class Neq(v0: Variable, v1: Variable) extends Constraint(Array(v0, v1)) {

  def checkValues(t: Array[Int]) = t(0) != t(1)

  def revise() = {
    val ch = revise(v0, v1) | revise(v1, v0)
    if (!isEntailed && (v0.dom disjoint v1.dom)) entail()
    ch
  }

  private def revise(variable: Variable, otherVar: Variable) = variable.dom.size == 1 && {
    val index = otherVar.dom.index(variable.dom.firstValue)
    if (index >= 0 && otherVar.dom.present(index)) {
      otherVar.dom.remove(index)
      entail()
      true
    } else false
  }

  override def toString = v0 + " /= " + v1

  override def getEvaluation = math.min(v0.dom.size, v1.dom.size)

  val simpleEvaluation = 2
}
