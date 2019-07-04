package concrete.constraint.semantic

import bitvectors.BitVector
import concrete._
import concrete.constraint.Constraint
import concrete.util.IntIntMap


class NotMember(variable: Variable, set: Array[Variable]) extends Constraint(variable +: set) {
  def advise(ps: ProblemState, event: Event, position: Int): Int = arity

  def check(tuple: Array[Int]): Boolean = {
    !tuple.tail.contains(tuple(0))
  }

  def init(ps: ProblemState): ProblemState = ps

  def simpleEvaluation: Int = 2

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    ps.fold(1 until arity) { (ps, i) =>
      val dom = ps.dom(scope(i))
      if (dom.isAssigned) {
        ps.remove(variable, dom.head)
      } else {
        ps
      }
    }
      .andThen { ps =>
        val dom = ps.dom(variable)
        if (dom.isAssigned) {
          val v = dom.head
          ps.fold(1 until arity) { (ps, i) => ps.removeIfPresent(scope(i), v) }
            .entail(this)
        } else {
          ps
        }
      }
  }
}

class Member(variable: Variable, set: Array[Variable]) extends Constraint(variable +: set) {

  /** residues(i) = j if scope(j) supports value i */
  private val residues = new IntIntMap(variable.initDomain.size)

  def advise(problemState: ProblemState, event: Event, position: Int): Int = {
    arity
  }

  def check(tuple: Array[Int]): Boolean = {
    val h = tuple.head
    tuple.view.tail.exists(_ == h)
  }

  def init(ps: ProblemState): Outcome = ps

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    def findNewSupport(i: Int) = {
      set.indices.find(j => ps.dom(set(j)).contains(i))
    }

    val r = ps.filterDom(variable) { i =>
      val validResidue = residues.get(i)
        .exists(w => ps.dom(set(w)).contains(i))

      validResidue || {
        val s = findNewSupport(i)
        for (found <- s) {
          residues.justPut(i, found)
        }
        s.isDefined
      }
    }
      .andThen { ps =>
        val dom = ps.dom(variable)

        // only one variable has an intersection with the result
        // => it is equal to the result.
        val find = set.iterator.filterNot(v => ps.dom(v).disjoint(dom))

        assert(find.hasNext)
        val first = find.next()
        if (find.hasNext) {
          ps
        } else {
          ps.intersectDom(first, dom)
        }

      }
      .entailIf(this, { ps =>
        val sure = set.iterator.flatMap { v =>
            val d = ps.dom(v)
            if (d.isAssigned) {
              Iterator(d.head)
            } else {
              Iterator.empty
            }
          }
          .toSet

        ps.dom(variable).subsetOf(sure)
      })

    r
  }

  def simpleEvaluation: Int = 2

}