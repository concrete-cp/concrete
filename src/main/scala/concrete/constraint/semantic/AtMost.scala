package concrete.constraint.semantic

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.{Constraint, Removals, StatefulConstraint}
import concrete.{Domain, Outcome, ProblemState, Variable}

class AtMost(val result: Variable, val value: Variable,
             val vars: Array[Variable])
  extends Constraint(result +: value +: vars) with StatefulConstraint[Map[Int, BitVector]] with Removals with LazyLogging {

  override def init(ps: ProblemState) = {
    val initMap = ps.dom(value).view.map(i => i -> BitVector.empty).toMap
    ps.updateState(this, updateState(ps, initMap, BitVector.filled(arity), ps.dom(value)))
  }

  def check(tuple: Array[Int]) = {
    tuple(0) >= (2 until arity).count(i => tuple(i) == tuple(1))
  }

  def getEvaluation(ps: ProblemState): Int = arity

  override def toString = s"At most $result occurrences of $value in (${vars.mkString(", ")})"

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val currentValues = ps.dom(value)

    val affected = updateState(ps, ps(this), mod, currentValues)

    filterResult(ps, currentValues, affected)
      .andThen(filterValue(_, affected))
      .andThen(filterVars(_, affected))
      .updateState(this, affected)
  }

  private def filterResult(ps: ProblemState, currentValues: Domain, affected: Map[Int, BitVector]) = {
    var min = Int.MaxValue
    currentValues.foreach(value => min = math.min(min, affected(value).cardinality))
    ps.removeUntil(result, min)
  }

  private def filterValue(ps: ProblemState, affected: Map[Int, BitVector]) = {
    val bound = ps.dom(result).last
    ps.filterDom(value) { v =>
      affected(v).cardinality <= bound
    }
  }

  private def filterVars(ps: ProblemState, affected: Map[Int, BitVector]) = {
    val values = ps.dom(value)

    if (values.isAssigned) {
      val value = values.head
      val bound = ps.dom(result).last

      if (affected(value).cardinality == bound) {
        // Maximum nb of values is affected, removing from other variables
        vars
          .foldLeft(ps) { (ps, v) =>
            val d = ps.dom(v)
            if (!d.isAssigned && d.present(value)) {
              ps.updateDomNonEmpty(v, d.remove(value))
            } else {
              ps
            }
          }
          .entail(this)
      } else {
        ps
      }
    } else {
      ps
    }
  }

  private def updateState(ps: ProblemState, affect: Map[Int, BitVector], mod: BitVector, currentValues: Domain): Map[Int, BitVector] = {
    var affected = affect
    var sm = mod.nextSetBit(2)
    while (sm >= 0) {
      val m = sm - 2
      val dom = ps.dom(vars(m))
      if (dom.isAssigned) {
        val value = dom.head
        /* Only process values that appear in the "value" variable */
        if (currentValues.present(value)) {
          affected = affected.updated(value, affected(value) + m)
        }
      }

      sm = mod.nextSetBit(sm + 1)
    }

    assert(
      currentValues.forall { v =>
        affected(v).traversable.toSeq == vars.indices.filter { p =>
          val d = ps.dom(vars(p))
          d.isAssigned && d.head == v
        }
      },

      s"incorrect affected for ${toString(ps)}: $affected after $mod")

    affected
  }

  override def toString(ps: ProblemState) = s"At most ${result.toString(ps)} occurrences of ${value.toString(ps)} in (${vars.map(_.toString(ps)).mkString(", ")})"

  def simpleEvaluation: Int = 3

}