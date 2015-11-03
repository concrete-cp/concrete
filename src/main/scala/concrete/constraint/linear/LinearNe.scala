package concrete.constraint.linear

import com.typesafe.scalalogging.LazyLogging
import concrete.Contradiction
import concrete.constraint.StatefulConstraint
import concrete.Variable
import concrete.ProblemState
import cspom.util.BitVector
import concrete.Outcome

final class LinearNe(
    constant: Int,
    factors: Array[Int],
    scope: Array[Variable]) extends Linear(constant, factors, scope, SumNE) with StatefulConstraint[(Int, BitVector)] with LazyLogging {

  private def totalSpan(ps: ProblemState, constant: Int, variables: BitVector): (BitVector, Int) = {
    var cons = constant
    val newVar = variables.filter { i =>
      val dom = ps.dom(scope(i))
      if (dom.size == 1) {
        cons -= dom.head * factors(i)
        false
      } else {
        true
      }
    }
    (newVar, cons)
  }

  override def isConsistent(ps: ProblemState) = {
    val (oldCons, oldVar) = ps(this)

    val (newVar, newCons) = totalSpan(ps, oldCons, oldVar)

    if (newVar.isEmpty && newCons == 0) {
      Contradiction
    } else if (oldVar == newVar) {
      ps
    } else {
      ps.updateState(this, (newCons, newVar))
    }
  }

  override def revise(ps: ProblemState): Outcome = {
    val (oldCons, oldVar) = ps(this)

    val (newVar, newCons) = totalSpan(ps, oldCons, oldVar)

    newVar.cardinality match {
      case 0 if (newCons == 0) => Contradiction
      case 1 =>
        val p = newVar.nextSetBit(0)
        ps.remove(scope(p), newCons / factors(p)).entail(this)
      case _ => ps.updateState(this, (newCons, newVar))
    }
  }

  override def toString() = toString("!=")

  override def toString(ps: ProblemState) = toString(ps, "!=")

  def advise(ps: ProblemState, p: Int) = arity * 2

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState) = {
    ps.updateState(this, (constant, BitVector.filled(arity)))
  }
}
