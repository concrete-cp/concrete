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

class AtLeast(val result: Variable, val value: Variable,
  val vars: Array[Variable])
    extends Constraint(result +: value +: vars) with StatefulConstraint[Map[Int, BitVector]] with Removals with LazyLogging {

  override def init(ps: ProblemState) =
    ps.updateState(this,
      value.initDomain.view.map(i => i -> BitVector.filled(vars.length)).toMap)

  def check(tuple: Array[Int]) = {
    tuple(0) <= (2 until arity).count(i => tuple(i) == tuple(1))
  }

  def getEvaluation(ps: ProblemState): Int = {
    val cba = ps(this)
    var s = 0
    for ((k, v) <- cba) {
      s += v.cardinality
    }
    s
  }

  override def toString = s"at least $result occurrences of $value in (${vars.mkString(", ")})"

  override def toString(ps: ProblemState) = s"at least ${result.toString(ps)} occurrences of ${value.toString(ps)} in (${vars.map(_.toString(ps)).mkString(", ")})"

  private def updateState(ps: ProblemState, mod: BitVector, currentValues: Domain): Map[Int, BitVector] = {
    var canBeAffectedSet = ps(this)

    var sm = mod.nextSetBit(2)
    while (sm >= 0) {
      val m = sm - 2
      val dom = ps.dom(vars(m))

      for (v <- currentValues) {
        if (canBeAffectedSet(v)(m) && !dom.present(v)) {
          canBeAffectedSet = canBeAffectedSet.updated(v, canBeAffectedSet(v) - m)
        }
      }
      sm = mod.nextSetBit(sm + 1)
    }

    canBeAffectedSet
  }

  private def filterResult(ps: ProblemState, currentValues: Domain, canBeAffectedSet: Map[Int, BitVector]) = {
    val max = currentValues.view.map(value => canBeAffectedSet(value).cardinality).max
    ps.removeAfter(result, max)
  }

  private def filterValue(ps: ProblemState, canBeAffectedSet: Map[Int, BitVector]) = {
    val bound = ps.dom(result).head
    ps.filterDom(value) { v =>
      canBeAffectedSet(v).cardinality >= bound
    }
  }

  private def filterVars(ps: ProblemState, canBeAffectedSet: Map[Int, BitVector]) = {
    val values = ps.dom(value)

    if (values.isAssigned) {
      val value = values.head
      val bound = ps.dom(result).head

      val cba = canBeAffectedSet(value).cardinality

      if (bound == cba) {
        // Remaining values must be affected
        canBeAffectedSet(value).traversable
          .foldLeft(ps) { (ps, p) =>
            val v = vars(p)
            val d = ps.dom(v)
            if (!d.isAssigned) {
              ps.updateDomNonEmpty(v, d.assign(value))
            } else {
              ps
            }
          }
          .entail(this)
      } else {
        ps.updateState(this, canBeAffectedSet)
      }

    } else {
      ps.updateState(this, canBeAffectedSet)
    }
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val currentValues = ps.dom(value)

    val canBeAffectedSet = updateState(ps, mod, currentValues)

    filterResult(ps, currentValues, canBeAffectedSet)
      .andThen(filterValue(_, canBeAffectedSet))
      .andThen(filterVars(_, canBeAffectedSet))
  }

  def simpleEvaluation: Int = 3

}