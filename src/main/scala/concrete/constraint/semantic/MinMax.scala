package concrete
package constraint
package semantic

import bitvectors.BitVector

abstract class MinMax(protected val result: Variable, protected val vars: Array[Variable]) extends Constraint(result +: vars) {

  def advise(ps: ProblemState, event: Event, pos: Int): Int = arity

  def simpleEvaluation: Int = 2

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} = ${this.getClass.getSimpleName}${vars.map(_.toString(ps)).mkString("(", ", ", ")")}"


  def doms(ps: ProblemState, from: Int, until: Int): Array[Domain] = {
    val d = new Array[Domain](vars.length)
    for (i <- from until until) {
      d(i) = ps.dom(vars(i))
    }
    d
  }
}

object Min {
  def apply(result: Variable, vars: Seq[Variable]) = new Min(result, vars.toArray.clone)
}

final class Min private(result: Variable, vars: Array[Variable])
  extends MinMax(result, vars) with StatefulConstraint[Seq[Variable]] {

  override def init(ps: ProblemState): Outcome = {
    val ub = ps.dom(result).last
    val list = vars.filter(v => ps.dom(v).head <= ub).toSeq
    ps.updateState(this, list)
  }


  def revise(ps: ProblemState, mod: BitVector): Outcome = {

    /*
     * Vars before "first" are strictly higher than minimum variable
     * (i.e., var.head > result.last)
     */
    var list: Seq[Variable] = ps(this)

    val resultDom = ps.dom(result)

    /*
     * Compute lb, ub
     */
    var lb = resultDom.last + 1
    var ub = resultDom.last

    list = list.filter { v =>
      val dom = ps.dom(v)
      lb = math.min(lb, dom.head)
      ub = math.min(ub, dom.last)
      dom.head <= ub
    }

    if (list.isEmpty) {
      Contradiction(result)
    } else {
      /*
       * Result must take values in variables' domain
       */
      val union = list.view.map(v => ps.dom(v) & (lb, ub)).reduce(_ | _) //foldLeft(EmptyIntDomain: Domain) { (u, v) => u | (ps.dom(vars(v)) & (lb, ub)) }

      val minDom = resultDom & union

      ps.updateDom(result, minDom)
        .andThen { ps =>
          var updated = ps
          list = list.filter { v =>
            val f = updated.dom(v).removeUntil(minDom.head)
            assert(f.nonEmpty)
            updated = updated.updateDomNonEmpty(v, f)
            f.head <= minDom.last
          }
          updated
        }
        .andThen { state =>

          list match {
            case Seq(single) =>
              /**
                * Handle case where only one variable can be the minimum
                */
              val intersection = minDom & ps.dom(single) //state.dom(v)
              state
                .updateDom(result, intersection)
                .updateDom(single, intersection)
            case _ => state
          }
        }

    }
      .updateState(this, list)
      .entailIfFree(this)


  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == tuple.view.slice(1, arity).min
  }

}

object Max {
  def apply(result: Variable, vars: Seq[Variable]) = new Max(result, vars.toArray.clone)
}

final class Max private(result: Variable, vars: Array[Variable])
  extends MinMax(result, vars) with StatefulConstraint[Seq[Variable]] {

  override def init(ps: ProblemState): Outcome = {
    val lb = ps.dom(result).head
    val list = vars.filter(v => ps.dom(v).last >= lb).toSeq
    ps.updateState(this, list)
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    /*
     * "list" contains variable positions that may be higher than result
     * (i.e., var.last >= result.head)
     */
    var list: Seq[Variable] = ps(this)

    val resultDom = ps.dom(result)

    /*
     * Compute lb, ub
     */
    var lb = resultDom.head
    var ub = resultDom.head - 1
    list = list.filter { v =>
      val dom = ps.dom(v)
      ub = math.max(ub, dom.last)
      lb = math.max(lb, dom.head)

      dom.last >= lb
    }

    if (list.isEmpty) {
      Contradiction(result)
    } else {

      /*
       * Result must take values in variables' domain
       */
      val union: Domain = list.view.map(v => ps.dom(v) & (lb, ub)).reduce(_ | _)
//      for (v <- list) {
//        union |= ps.dom(v) & (lb, ub)
//      }

      val maxDom = resultDom & union


      ps.updateDom(result, maxDom)
        .andThen { ps =>
          var updated = ps
          list = list.filter { v =>
            val f = ps.dom(v).removeAfter(maxDom.last)
            assert(f.nonEmpty)
            updated = updated.updateDomNonEmpty(v, f)
            f.last >= maxDom.head
          }
          updated
        }
        .andThen { state =>

          /**
            * Handle case where only one variable can be the minimum
            */
          list match {
            case Seq(single) =>
              val intersection = maxDom & ps.dom(single)
              state
                .updateDom(result, intersection)
                .updateDom(single, intersection)
            case _ => state
          }

        }
        .updateState(this, list)
        .entailIfFree(this)
    }

  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == tuple.view.slice(1, arity).max
  }

}