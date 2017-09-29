package concrete
package constraint
package semantic

import scala.annotation.tailrec

abstract class MinMax(protected val result: Variable, protected val vars: Array[Variable]) extends Constraint(result +: vars) {

  def advise(ps: ProblemState, event: Event, pos: Int): Int = arity

  def simpleEvaluation: Int = 2

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} = ${this.getClass.getSimpleName}${vars.map(_.toString(ps)).mkString("(", ", ", ")")}"

  def swap[T](a: Array[T], i: Int, j: Int): Unit = {
    val t = a(i)
    a(i) = a(j)
    a(j) = t
  }

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
  extends MinMax(result, vars) with StatefulConstraint[Integer] {

  override def init(ps: ProblemState): Outcome = {
    var first = 0
    val ub = ps.dom(result).last
    for (i <- vars.indices) {
      if (ps.dom(vars(i)).head > ub) {
        swap(vars, i, first)
        first += 1
      }
    }
    ps.updateState(this, Int.box(first))
  }


  def revise(ps: ProblemState): Outcome = {

    /*
     * Vars before "first" are strictly higher than minimum variable
     * (i.e., var.head > result.last)
     */
    var first: Int = ps(this)

    val resultDom = ps.dom(result)

    /*
     * Compute lb, ub
     */
    var lb = resultDom.last + 1
    var ub = Int.MaxValue

    for (p <- first until vars.length) {
      val dom = ps.dom(vars(p))
      lb = math.min(lb, dom.head)
      ub = math.min(ub, dom.last)
      if (dom.head > ub) {
        swap(vars, p, first)
        first += 1
      }

    }

    if (first == vars.length) {
      Contradiction(result)
    } else {

      /*
       * Result must take values in variables' domain
       */
      var union: Domain = EmptyIntDomain
      for (v <- first until vars.length) {
        union |= ps.dom(vars(v)) & (lb, ub)
      }

      val minDom = resultDom & union


      @tailrec
      def filterVariables(ps: ProblemState, i: Int): Outcome = {
        if (i >= vars.length) {
          ps
        } else {

          val f = ps.dom(vars(i)).removeUntil(minDom.head)

          if (f.isEmpty) {
            Contradiction(vars(i))
          } else {
            val updated = ps.updateDomNonEmpty(vars(i), f)
            if (f.head > minDom.last) {
              swap(vars, i, first)
              first += 1
            }
            filterVariables(updated, i + 1)
          }
        }
      }

      ps.updateDom(result, minDom)
        .andThen(filterVariables(_, first))
        .andThen { state =>

          val singletonTest1 = Iterator.range(first, vars.length).filterNot(p => ps.dom(vars(p)) disjoint minDom).take(2).size == 1
          val singletonTest2 = first == vars.length - 1

          assert(singletonTest1 == singletonTest2)
          /**
            * Handle case where only one variable can be the minimum
            */
          if (singletonTest1) {

            val intersection = minDom & ps.dom(vars(first)) //state.dom(v)
            state
              .updateDom(result, intersection)
              .updateDom(vars(first), intersection)
          } else {
            state
          }
        }

    }
      .updateState(this, Int.box(first))
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
  extends MinMax(result, vars) with StatefulConstraint[Integer] {

  override def init(ps: ProblemState): Outcome = {
    var first = 0
    val lb = ps.dom(result).head
    for (i <- vars.indices) {
      if (ps.dom(vars(i)).last < lb) {
        swap(vars, i, first)
        first += 1
      }
    }
    ps.updateState(this, Int.box(first))
  }

  def revise(ps: ProblemState): Outcome = {
    /*
     * Vars before "first" are strictly lower than maximum variable
     * (i.e., var.last < result.head)
     */
    var first: Int = ps(this)

    val resultDom = ps.dom(result)

    /*
     * Compute lb, ub
     */
    var lb = Int.MinValue
    var ub = resultDom.head - 1

    for (p <- first until vars.length) {
      val dom = ps.dom(vars(p))
      ub = math.max(ub, dom.last)
      lb = math.max(lb, dom.head)

      if (dom.last < lb) {
        swap(vars, p, first)
        first += 1
      }
    }

    if (first == vars.length) {
      Contradiction(result)
    } else {

      /*
       * Result must take values in variables' domain
       */
      var union: Domain = EmptyIntDomain
      for (v <- first until vars.length) {
        union |= ps.dom(vars(v)) & (lb, ub)
      }

      val maxDom = resultDom & union



      def filterVariables(ps: ProblemState, i: Int): Outcome = {
        if (i >= vars.length) {
          ps
        } else {

          val f = ps.dom(vars(i)).removeAfter(maxDom.last)

          if (f.isEmpty) {
            Contradiction(vars(i))
          } else {
            val updated = ps.updateDomNonEmpty(vars(i), f)
            if (f.last < maxDom.head) {
              swap(vars, i, first)
              first += 1
            }
            filterVariables(updated, i + 1)
          }
        }
      }

      ps.updateDom(result, maxDom)
        .andThen(filterVariables(_, first))
        .andThen { state =>

          val singletonTest1 = Iterator.range(first, vars.length).filterNot(p => ps.dom(vars(p)) disjoint maxDom).take(2).size == 1
          val singletonTest2 = first == vars.length - 1

          assert(singletonTest1 == singletonTest2)
          /**
            * Handle case where only one variable can be the minimum
            */
          if (singletonTest1) {
            val intersection = maxDom & ps.dom(vars(first))
            state
              .updateDom(result, intersection)
              .updateDom(vars(first), intersection)
          } else {
            state
          }

        }
        .updateState(this, Int.box(first))
        .entailIfFree(this)
    }

  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == tuple.view.slice(1, arity).max
  }

}