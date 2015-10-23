package concrete.constraint.semantic

import scala.runtime.RichInt
import concrete.Contradiction
import concrete.Domain
import concrete.EmptyIntDomain
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.BC
import concrete.constraint.Constraint
import concrete.constraint.StatefulConstraint
import cspom.util.IntInterval
import concrete.IntDomain

abstract class MinMax(protected val result: Variable, protected val vars: Array[Variable]) extends Constraint(result +: vars) {

  def advise(ps: ProblemState, pos: Int): Int = arity

  def simpleEvaluation: Int = 2

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} = ${this.getClass.getSimpleName}${vars.map(_.toString(ps)).mkString("(", ", ", ")")}"

  protected def swap(i: Int, j: Int): Unit = {
    val t = vars(i)
    vars(i) = vars(j)
    vars(j) = t
  }
}

object Min {
  def apply(result: Variable, vars: Seq[Variable]) = new Min(result, vars.toArray.clone)
}

final class Min private (result: Variable, _vars: Array[Variable])
    extends MinMax(result, _vars) with StatefulConstraint[Integer] {

  override def init(ps: ProblemState): Outcome = {
    var first = 0
    val ub = ps.dom(result).last
    for (i <- 0 until vars.length) {
      if (ps.dom(vars(i)).head > ub) {
        swap(i, first)
        first += 1
      }
    }
    ps.updateState(this, Integer.valueOf(first))
  }

  def revise(ps: ProblemState): Outcome = {

    /**
     * Vars before "first" are strictly higher than minimum variable
     * (i.e., var.head > result.last)
     */
    var first: Int = ps(this).intValue

    val resultDom = ps.dom(result)
    /**
     * Compute lb, ub
     */
    var lb = resultDom.last + 1
    var ub = Int.MaxValue

    for (p <- first until vars.length) {
      val v = vars(p)
      val dom = ps.dom(v)

      val intersect = resultDom & dom
      if (intersect.nonEmpty) {
        lb = math.min(lb, intersect.head)
      }
      ub = math.min(ub, dom.last)
      if (dom.head > ub) {
        swap(p, first)
        first += 1
      }

    }

    if (first == vars.length) {
      Contradiction
    } else {

      /**
       * Result must take values in variables' domain
       */
      var union: Domain = EmptyIntDomain
      for (v <- vars) {
        union |= (ps.dom(v) & (lb, ub))
      }

      val minDom = resultDom & union

      ps.updateDom(result, minDom)
        .andThen { ps =>

          var state: Outcome = ps
          var i = first

          /**
           * Filter variables
           */
          while ((state ne Contradiction) && i < vars.length) {
            val v = vars(i)
            val d = ps.dom(v)
            val f = d.removeUntil(minDom.head)

            if (f.isEmpty) {
              state = Contradiction
            } else {
              state = state.asInstanceOf[ProblemState].updateDomNonEmpty(v, f)
              if (f.head > minDom.last) {
                swap(i, first)
                first += 1
              }
            }
            i += 1
          }

          /**
           * Handle case where only one variable can be the minimum
           */
          if ((state ne Contradiction) && (first until vars.length).iterator.filter(p => (state.dom(vars(p)) & minDom).nonEmpty).take(2).size == 1) {

            val v = vars(first)
            val intersection = minDom & state.dom(v)
            state
              .updateDom(result, intersection)
              .updateDom(v, intersection)
          } else {
            state
          }

        }
        .updateState(this, Integer.valueOf(first))
        .entailIfFree(this)
    }

  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == tuple.view.slice(1, arity).min
  }

}

object Max {
  def apply(result: Variable, vars: Seq[Variable]) = new Max(result, vars.toArray.clone)
}

final class Max private (result: Variable, _vars: Array[Variable])
    extends MinMax(result, _vars) with StatefulConstraint[Integer] {

  override def init(ps: ProblemState): Outcome = {
    var first = 0
    val lb = ps.dom(result).head
    for (i <- 0 until vars.length) {
      if (ps.dom(vars(i)).last < lb) {
        swap(i, first)
        first += 1
      }
    }
    ps.updateState(this, Integer.valueOf(first))
  }

  def revise(ps: ProblemState): Outcome = {

    /**
     * Vars before "first" are strictly lower than maximum variable
     * (i.e., var.last < result.head)
     */
    var first: Int = ps(this).intValue

    val resultDom = ps.dom(result)
    /**
     * Compute lb, ub
     */
    var lb = Int.MinValue
    var ub = resultDom.head - 1

    for (p <- first until vars.length) {
      val v = vars(p)
      val dom = ps.dom(v)

      val intersect = resultDom & dom
      if (intersect.nonEmpty) {
        ub = math.max(ub, intersect.last)
      }
      lb = math.max(lb, dom.head)
      if (dom.last < lb) {
        swap(p, first)
        first += 1
      }

    }

    if (first == vars.length) {
      Contradiction
    } else {

      /**
       * Result must take values in variables' domain
       */
      var union: Domain = EmptyIntDomain
      for (v <- vars) {
        union |= (ps.dom(v) & (lb, ub))
      }

      val maxDom = resultDom & union

      ps.updateDom(result, maxDom)
        .andThen { ps =>

          var state: Outcome = ps
          var i = first

          /**
           * Filter variables
           */
          while ((state ne Contradiction) && i < vars.length) {
            val v = vars(i)
            val d = ps.dom(v)
            val f = d.removeAfter(maxDom.last)

            if (f.isEmpty) {
              state = Contradiction
            } else {
              state = state.asInstanceOf[ProblemState].updateDomNonEmpty(v, f)
              if (f.last < maxDom.head) {
                swap(i, first)
                first += 1
              }
            }
            i += 1
          }

          /**
           * Handle case where only one variable can be the minimum
           */
          if ((state ne Contradiction) && (first until vars.length).iterator.filter(p => (state.dom(vars(p)) & maxDom).nonEmpty).take(2).size == 1) {

            val v = vars(first)
            val intersection = maxDom & state.dom(v)
            state
              .updateDom(result, intersection)
              .updateDom(v, intersection)
          } else {
            state
          }

        }
        .updateState(this, Integer.valueOf(first))
        .entailIfFree(this)
    }

  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == tuple.view.slice(1, arity).max
  }

}