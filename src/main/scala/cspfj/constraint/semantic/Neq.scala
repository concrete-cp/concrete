package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

final class Neq(v0: Variable, v1: Variable) extends AbstractConstraint(Array(v0, v1)) {

  val corresponding0 =
    v1.dom.values map v0.dom.index toIndexedSeq

  val corresponding1 =
    v0.dom.values map v1.dom.index toIndexedSeq

  def check = value(0) != value(1)

  def revise(revisionHandler: RevisionHandler, revisionCount: Int): Boolean =
    revise(revisionHandler, v0, v1, corresponding1) &&
      revise(revisionHandler, v1, v0, corresponding0)

  def revise(revisionHandler: RevisionHandler, variable: Variable, otherVar: Variable, corresponding: IndexedSeq[Int]): Boolean = {
    if (variable.dom.size == 1) {
      val index = corresponding(variable.dom.first)
      if (index >= 0 && otherVar.dom.present(index)) {
        if (otherVar.dom.size == 1) return false
        otherVar.dom.remove(index)
        revisionHandler.revised(this, otherVar)
        entail()
      }
    }
    true
  }

  def toString = v0 + " /= " + v1

  val getEvaluation = arity
}
