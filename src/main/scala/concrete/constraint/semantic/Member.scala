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

  /** watched(i) contains j if scope(i) supports value j */
  // private val watched = new mutable.HashMap[Int, mutable.Set[Int]]() with mutable.MultiMap[Int, Int]

  def advise(problemState: ProblemState, event: Event, position: Int): Int = {
    arity
  }

  def check(tuple: Array[Int]): Boolean = {
    tuple.tail.contains(tuple(0))
  }

  def init(ps: ProblemState): Outcome = ps

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    def findNewSupport(i: Int) = {
      (1 until arity).find(j => ps.dom(scope(j)).contains(i))
    }

    val r = ps.filterDom(variable) { i =>
      val validResidue = residues.get(i)
        .exists(w => ps.dom(scope(w)).contains(i))

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
        val find = Iterator.range(1, arity).filterNot(i => ps.dom(scope(i)).disjoint(dom))
        assert(find.hasNext)
        val first = find.next()
        if (find.hasNext) {
          ps
        } else {
          ps.intersectDom(scope(first), dom)
        }

      }
      .entailIf(this, { ps =>
        val dom = ps.dom(variable)
        dom.isAssigned && (1 until arity).exists { i =>
          val d = ps.dom(scope(i))
          d.isAssigned && dom.head == d.head
        }
      })

    r
  }

  def simpleEvaluation: Int = 2

}