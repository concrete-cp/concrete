package concrete
package constraint
package linear

import com.typesafe.scalalogging.LazyLogging

import cspom.util.BitVector

final class LinearNe(
    constant: Int,
    factors: Array[Int],
    scope: Array[Variable]) extends Linear(constant, factors, scope, SumNE) with StatefulConstraint[(Int, BitVector)] with LazyLogging {

  private def totalSpan(ps: ProblemState, constant: Int, variables: BitVector): (BitVector, Int) = {
    var cons = constant
    val newVar = variables.filter { i =>
      val dom = ps.dom(scope(i))
      if (dom.isAssigned) {
        cons -= dom.head * factors(i)
        false
      } else {
        true
      }
    }
    (newVar, cons)
  }

  override def consistent(ps: ProblemState) = {
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

    val first = newVar.nextSetBit(0)
    if (first < 0) {
      if (newCons == 0) {
        Contradiction
      } else {
        ps.entail(this)
      }
    } else {
      val second = newVar.nextSetBit(first + 1)
      if (second < 0) {
        ps.removeIfPresent(scope(first), newCons / factors(first)).entail(this)
      } else {
        ps.updateState(this, (newCons, newVar))
      }
    }

    //    newVar.cardinality match {
    //      case 0 if (newCons == 0) => Contradiction
    //      case 1 =>
    //        val p = newVar.nextSetBit(0)
    //        ps.removeIfPresent(scope(p), newCons / factors(p)).entail(this)
    //      case _ => ps.updateState(this, (newCons, newVar))
    //    }
  }

  override def toString() = toString("!=")

  override def toString(ps: ProblemState) = toString(ps, "!=")

  def advise(ps: ProblemState, event: Event, p: Int) = arity * 2

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState) = {
    ps.updateState(this, (constant, BitVector.filled(arity)))
  }
}
