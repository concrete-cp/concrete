package concrete.constraint.semantic

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete._
import concrete.constraint.{Constraint, StatefulConstraint}

class AtLeast(val result: Variable, val value: Variable,
              val vars: Array[Variable])
  extends Constraint(result +: value +: vars) with StatefulConstraint[Map[Int, BitVector]] with LazyLogging {

  override def init(ps: ProblemState): ProblemState =
    ps.updateState(this,
      value.initDomain.view.map(i => i -> BitVector.filled(vars.length)).toMap)

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) <= (2 until arity).count(i => tuple(i) == tuple(1))
  }

  def advise(ps: ProblemState, event: Event, pos: Int): Int = {
    val cba = ps(this)
    var s = 0
    for (v <- cba.valuesIterator) {
      s += v.cardinality
    }
    s
  }

  override def toString = s"at least $result occurrences of $value in (${vars.mkString(", ")})"

  override def toString(ps: ProblemState) = s"at least ${result.toString(ps)} occurrences of ${value.toString(ps)} in (${vars.map(_.toString(ps)).mkString(", ")})"

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val currentValues = ps.dom(value)

    val canBeAffectedSet = updateState(ps, mod, currentValues)

    filterResult(ps, currentValues, canBeAffectedSet)
      .andThen(filterValue(_, canBeAffectedSet))
      .andThen(filterVars(_, canBeAffectedSet))
  }

  private def updateState(ps: ProblemState, mod: BitVector, currentValues: Domain): Map[Int, BitVector] = {
    var canBeAffectedSet = ps(this)

    var sm = mod.nextSetBit(2)
    while (sm >= 0) {
      val m = sm - 2
      val dom = ps.dom(vars(m))

      for (v <- currentValues) {

        val cba = canBeAffectedSet(v)
        logger.debug(s"check value $v from variable $m : ${cba(m)}, ${dom.present(v)}")
        if (cba(m) && !dom.present(v)) {
          canBeAffectedSet = canBeAffectedSet.updated(v, cba - m)
        }
      }
      sm = mod.nextSetBit(sm + 1)
    }

    assert (
      currentValues.forall( v=> canBeAffectedSet(v).forall(m => ps.dom(vars(m)).present(v))),
      s"${toString(ps)}, $canBeAffectedSet is incorrect, mod = $mod"
    )

    canBeAffectedSet
  }

  private def filterResult(ps: ProblemState, currentValues: Domain, canBeAffectedSet: Map[Int, BitVector]) = {
    var max = 0
    currentValues.foreach(value => max = math.max(max, canBeAffectedSet(value).cardinality))
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
        canBeAffectedSet(value)
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

  def simpleEvaluation: Int = 3

}