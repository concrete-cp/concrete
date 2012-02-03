package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.problem.Domain
import cspfj.problem.Variable

final class Neq(v0: Variable, v1: Variable) extends AbstractConstraint(Array(v0, v1)) {

  //private val corresponding0 = v1.dom.allValues map v0.dom.index

  //private val corresponding1 = v0.dom.allValues map v1.dom.index

  def check = value(0) != value(1)

  def revise(revisionCount: Int): Boolean = {
    val r = revise(v0, v1) && revise(v1, v0)
    if (v0.dom.disjoint(v1.dom)) entail()
    r
  }

  private def revise(variable: Variable, otherVar: Variable) = {
    if (variable.dom.size == 1) {
      val index = otherVar.dom.index(variable.dom.firstValue) //corresponding(variable.dom.first)
      if (index >= 0 && otherVar.dom.present(index)) {
        if (otherVar.dom.size == 1) false
        else {
          otherVar.dom.remove(index)
          true
        }
      } else true
    } else true
  }

  override def toString = v0 + " /= " + v1

  override val getEvaluation = 2.0
}
