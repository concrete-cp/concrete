package concrete
package constraint
package semantic

import bitvectors.BitVector
import concrete.util.Interval

abstract class MinMax(protected val result: Variable, protected val vars: Array[Variable]) extends Constraint(result +: vars) with BC
  with StatefulConstraint[Seq[Variable]] with FixPoint {

  override def init(ps: ProblemState): Outcome = {
    val resultDom = ps.dom(result)
    val list = vars.filter(v => isAlive(ps.dom(v), resultDom)).toSeq
    if (list.isEmpty) {
      Contradiction(Seq())
    } else {
      ps.updateState(this, list)
    }
  }

  def advise(ps: ProblemState, pos: Int): Int = arity

  def simpleEvaluation: Int = 2

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} = ${this.getClass.getSimpleName}(${vars.map(_.toString(ps)).mkString(", ")})"

  def isAlive(d: Domain, resultDom: Domain): Boolean

  def bounds(ps: ProblemState, list: Seq[Variable]): Interval

  def shaveDomain(ps: Outcome, v: Variable, r: Domain): Domain

  def revise(ps: ProblemState, mod: BitVector): Outcome = fixPoint(ps, shave(_))

  def shave(ps: ProblemState): Outcome = {

    /*
     * list contains vars that may be lower than minimum variable
     * (i.e., var.head <= result.last)
     */
    val readList: Seq[Variable] = ps(this)


    /*
     * Compute lb, ub
     */
    val resultDom = ps.dom(result) & bounds(ps, readList)

    ps.updateDom(result, resultDom)
      .andThen { state =>
        var updated: Outcome = state

        val list = readList.filter { v =>
          updated.isState && {
            val dom = shaveDomain(updated, v, resultDom) //.dom(v).removeUntil(resultDom.head)
            updated = updated.updateDom(v, dom)
            dom.nonEmpty && isAlive(dom, resultDom)
          }
        }

        if (list.isEmpty) {
          Contradiction(result)
        } else if (list.lengthCompare(1) == 0) {

          val single = list.head

          /**
            * Handle case where only one variable can be the minimum
            */
          // val intersection = resultDom & ps.span(single) //state.dom(v)
          updated
            .shaveDom(result, ps.span(single))
            .shaveDom(single, resultDom.span)
            .entailIf(this, ps => ps.dom(result).isAssigned)
            .updateState(this, list)
        } else {
          updated.updateState(this, list)
        }


      }
  }

}

object Min {
  def apply(result: Variable, vars: Seq[Variable]) = new Min(result, vars.toArray.clone)
}

final class Min private(result: Variable, vars: Array[Variable])
  extends MinMax(result, vars) {


  def isAlive(d: Domain, resultDom: Domain): Boolean = {
    d.head <= resultDom.last
  }

  def bounds(ps: ProblemState, ds: Seq[Variable]): Interval = {
    var lb = Int.MaxValue
    var ub = Int.MaxValue
    for (v <- ds) {
      val d = ps.dom(v)
      lb = math.min(lb, d.head)
      ub = math.min(ub, d.last)
    }
    Interval(lb, ub)
  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == tuple.view.slice(1, arity).min
  }

  def shaveDomain(ps: Outcome, v: Variable, r: Domain): Domain = {
    ps.dom(v).removeUntil(r.head)
  }

}

final class Max private(result: Variable, vars: Array[Variable])
  extends MinMax(result, vars) with StatefulConstraint[Seq[Variable]] {

  def isAlive(d: Domain, resultDom: Domain): Boolean = {
    d.last >= resultDom.head
  }

  def bounds(ps: ProblemState, ds: Seq[Variable]): Interval = {
    var lb = Int.MinValue
    var ub = Int.MinValue
    for (v <- ds) {
      val d = ps.dom(v)
      lb = math.max(lb, d.head)
      ub = math.max(ub, d.last)
    }
    Interval(lb, ub)
  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == tuple.view.slice(1, arity).max
  }

  def shaveDomain(ps: Outcome, v: Variable, r: Domain): Domain = {
    ps.dom(v).removeAfter(r.last)
  }

}

object Max {
  def apply(result: Variable, vars: Seq[Variable]) = new Max(result, vars.toArray.clone)
}