package concrete.constraint.semantic

import com.typesafe.scalalogging.LazyLogging

import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Removals
import concrete.constraint.StatefulConstraint
import cspom.util.BitVector

class Occurrence(val result: Variable, val value: Variable,
                 val vars: Array[Variable])
    extends Constraint(result +: value +: vars) with StatefulConstraint[(Map[Int, Int], Map[Int, BitVector])] with Removals with LazyLogging {

  override def init(ps: ProblemState) =
    ps.updateState(this, (
      value.initDomain.view.map(i => i -> 0).toMap,
      value.initDomain.view.map(i => i -> BitVector.filled(vars.length)).toMap))

  def check(tuple: Array[Int]) = {
    tuple(0) == (2 until arity).count(i => tuple(i) == tuple(1))
  }

  def getEvaluation(ps: ProblemState): Int = {
    val (_, cba) = ps(this)
    var s = 0
    for ((k, v) <- cba) {
      s += v.cardinality
    }
    s
  }

  override def toString = s"$result occurrences of $value in (${vars.mkString(", ")})"

  override def toString(ps: ProblemState) = s"${result.toString(ps)} occurrences of ${value.toString(ps)} in (${vars.map(_.toString(ps)).mkString(", ")})"

  private def updateState(ps: ProblemState, mod: BitVector, currentValues: Domain): (Map[Int, Int], Map[Int, BitVector]) = {
    var (affected, canBeAffectedSet) = ps(this)

    var sm = mod.nextSetBit(2)
    while (sm >= 0) {
      val m = sm - 2

      for (v <- currentValues) {
        if (canBeAffectedSet(v)(m)) {
          val dom = ps.dom(vars(m))
          if (!dom.present(v)) {
            canBeAffectedSet = canBeAffectedSet.updated(v, canBeAffectedSet(v) - m)
          } else if (dom.isAssigned) {
            canBeAffectedSet = canBeAffectedSet.updated(v, canBeAffectedSet(v) - m)
            affected = affected.updated(v, affected(v) + 1)
          }
        }
      }
      sm = mod.nextSetBit(sm + 1)
    }

    (affected, canBeAffectedSet)
  }

  private def filterResult(ps: ProblemState, currentValues: Domain, affected: Map[Int, Int], canBeAffectedSet: Map[Int, BitVector]) =
    ps.filterDom(result) { v =>
      currentValues.exists { value =>
        val a = affected(value)
        a <= v && v <= a + canBeAffectedSet(value).cardinality
      }
    }

  private def filterValue(ps: ProblemState, affected: Map[Int, Int], canBeAffectedSet: Map[Int, BitVector]) = {
    val resultDom = ps.dom(result)
    ps.filterDom(value) { v =>
      val a = affected(v)
      (resultDom & (a, a + canBeAffectedSet(v).cardinality)).nonEmpty
    }
  }

  private def filterVars(ps: ProblemState, affected: Map[Int, Int], canBeAffectedSet: Map[Int, BitVector]) = {
    val values = ps.dom(value)

    if (values.size == 1) {
      val value = values.head
      val result = ps.dom(this.result)
      val a = affected(value)
      val cba = canBeAffectedSet(value).cardinality
      var state = ps
      if (a == result.last && cba > 0) {
        // Maximum of values are affected, removing from other variables
        val cba = canBeAffectedSet(value)
        for (p <- cba.iterator) {
          val v = vars(p)
          state = state.updateDomNonEmpty(v, state.dom(v).remove(value))
        }

        state.entail(this).updateState(
          this, (affected.updated(value, result.last), canBeAffectedSet.updated(value, BitVector.empty)))
      } else if (result.head == a + cba) {
        // Remaining values must be affected
        for (p <- canBeAffectedSet(value).iterator) {
          val v = vars(p)
          state = state.updateDomNonEmpty(v, state.dom(v).assign(value))
        }
        state.entail(this).updateState(
          this, (affected.updated(value, result.head), canBeAffectedSet.updated(value, BitVector.empty)))
      } else {
        state.updateState(this, (affected, canBeAffectedSet))
      }

    } else {
      ps.updateState(this, (affected, canBeAffectedSet))
    }
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    //println(toString(ps))

    val currentValues = ps.dom(value)

    val (affected, canBeAffectedSet) = updateState(ps, mod, currentValues)

    filterResult(ps, currentValues, affected, canBeAffectedSet)
      .andThen(filterValue(_, affected, canBeAffectedSet))
      .andThen(filterVars(_, affected, canBeAffectedSet))

  }

  def simpleEvaluation: Int = 3

}