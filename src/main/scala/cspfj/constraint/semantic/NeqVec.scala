package cspfj.constraint.semantic;

import cspfj.constraint.Constraint
import cspfj.Domain
import cspfj.Variable

final class NeqVec(x: Seq[Variable], y: Seq[Variable]) extends Constraint((x ++ y).toArray) {
  require(x.length == y.length)

  val zip = x.zip(y)

  def checkValues(t: Array[Int]) = t(0) != t(1)

  def revise() = {
    var ch = false
    if (x.forall(_.dom.size == 1)) {
      for ((vx, vy) <- zip) {
        ch |= revise(vx, vy)
      }
      entail()
    } else if (y.forall(_.dom.size == 1)) {
      for ((vx, vy) <- zip) {
        ch |= revise(vy, vx)
      }
      entail()
    }
    ch
  }

  private def revise(variable: Variable, otherVar: Variable) = {
    val index = otherVar.dom.index(variable.dom.firstValue)
    if (index >= 0 && otherVar.dom.present(index)) {
      otherVar.dom.remove(index)
      true
    } else false
  }

  override def toString = x.toSeq + " /= " + y.toSeq

  def advise(p: Int) = arity

  val simpleEvaluation = 2
}
