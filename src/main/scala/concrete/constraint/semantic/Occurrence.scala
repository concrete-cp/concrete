package concrete.constraint.semantic

import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.Singleton
import concrete.Contradiction
import concrete.ProblemState
import concrete.Outcome
import concrete.constraint.StatefulConstraint
import concrete.constraint.Removals
import concrete.util.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete.util.Interval

class Occurrence(val result: Variable, val value: Variable,
                 val vars: Array[Variable])
  extends Constraint(result +: value +: vars) with StatefulConstraint with Removals with LazyLogging {

  type State = (Map[Int, Int], Map[Int, BitVector])

  def initState = (value.initDomain.map(i => i -> 0).toMap, value.initDomain.map(i => i -> BitVector.filled(vars.length)).toMap)

  def check(tuple: Array[Int]) = {
    tuple(0) == (2 until arity).count(i => tuple(i) == tuple(1))
  }

  def getEvaluation(ps: ProblemState): Int = scopeSize(ps)

  override def toString = s"$result occurrences of $value in (${vars.mkString(", ")})"

  override def toString(ps: ProblemState) = s"${result.toString(ps)} occurrences of ${value.toString(ps)} in (${vars.map(_.toString(ps)).mkString(", ")})"

  def revise(ps: ProblemState, mod: List[Int]): Outcome = {
    //println(toString(ps))

    var (affected, canBeAffectedSet) = state(ps)

    val currentValues = ps.dom(value)

    for (sm <- mod) {
      val m = sm - 2
      if (m >= 0) {
        for (v <- currentValues) {
          if (canBeAffectedSet(v)(m)) {
            val dom = ps.dom(vars(m))
            if (!dom.present(v)) {
              canBeAffectedSet = canBeAffectedSet.updated(v, canBeAffectedSet(v) - m)
            } else if (dom.size == 1) {
              canBeAffectedSet = canBeAffectedSet.updated(v, canBeAffectedSet(v) - m)
              affected = affected.updated(v, affected(v) + 1)
            }
          }
        }
      }
    }

    ps.filterDom(result) { v =>
      currentValues.exists { value =>
        val a = affected(value)
        a <= v && v <= a + canBeAffectedSet(value).cardinality
      }
    }
      .andThen {
        ps =>
          val resultDom = ps.dom(result)
          ps.filterDom(value) { v =>
            val a = affected(v)
            (resultDom & (a, a + canBeAffectedSet(v).cardinality)).nonEmpty
          }
      }
      .andThen {
        ps =>
          val values = ps.dom(value)
          if (values.size == 1) {
            val value = values.head
            val result = ps.dom(this.result)
            val a = affected(value)
            val cba = canBeAffectedSet(value).cardinality
            var state = ps
            if (a == result.last && cba > 0) {
              // Maximum of values are affected, removing from other variables
              var cba = canBeAffectedSet(value)
              for (p <- cba.iterator) {
                val v = vars(p)
                state = state.updateDomNonEmpty(v, state.dom(v).remove(value))
              }
              affected = affected.updated(value, result.last)
              canBeAffectedSet = canBeAffectedSet.updated(value, BitVector.empty)

              state.entail(this)
            } else if (result.head == a + cba) {
              // Remaining values must be affected
              for (p <- canBeAffectedSet(value).iterator) {
                val v = vars(p)
                state = state.updateDomNonEmpty(v, state.dom(v).assign(value))
              }
              affected = affected.updated(value, result.head)
              canBeAffectedSet = canBeAffectedSet.updated(value, BitVector.empty)
              state.entail(this)
            }
            state

          } else {
            ps
          }
      }
      .updateState(this, (affected, canBeAffectedSet))

    //    andThen {
    //      ps =>
    //            val result = ps.dom(this.result)
    //            if (affected == result.last && canBeAffected > 0) {
    //              var state = ps
    //              for (p <- canBeAffectedSet.iterator) {
    //                val v = vars(p)
    //                state = state.updateDomNonEmpty(v, state.dom(v).remove(value))
    //              }
    //              //canBeAffectedSet = BitVector.empty
    //    
    //              state.entail(this)
    //              //          ps.updateAll(vars) { d =>
    //              //            if (d.size > 1) d.remove(value) else d
    //              //          }
    //            } else if (result.head == affected + canBeAffected) {
    //              //affected = result.head
    //              var state = ps
    //              for (p <- canBeAffectedSet.iterator) {
    //                val v = vars(p)
    //                state = state.updateDomNonEmpty(v, state.dom(v).assign(value))
    //              }
    //              //canBeAffectedSet = BitVector.empty
    //              state.entail(this)
    //              //          ps.updateAll(vars) { d =>
    //              //            if (d.size > 1 && d.present(value)) d.assign(value) else d
    //              //          }
    //    //
    //    //        } else {
    //    //          ps.updateState(this, (affected, canBeAffectedSet))
    //    //        }

  }

  def simpleEvaluation: Int = ???

}