package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.Domain
import concrete.Variable

final class Neq(v0: Variable, v1: Variable) extends Constraint(Array(v0, v1)) {

  def checkValues(t: Array[Int]) = t(0) != t(1)

  def revise() = {
    var ch: List[Int] = Nil
    if (revise(v0, v1)) {
      ch ::= 1
    }
    if (revise(v1, v0)) {
      ch ::= 0
    }

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

  override def isConsistent = {
    v0.dom.size > 1 || v1.dom.size > 1 || v0.dom.firstValue != v1.dom.firstValue
  }

  override def toString = v0 + " /= " + v1

  def advise(p: Int) = if (scope(p).dom.size > 1) -1 else math.min(v0.dom.size, v1.dom.size)

  val simpleEvaluation = 2
}
