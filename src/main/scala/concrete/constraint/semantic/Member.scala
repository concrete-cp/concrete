package concrete.constraint.semantic

import bitvectors.BitVector
import concrete.Assignment
import concrete.Event
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint

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

  /** watches(i) = j if scope(j) supports value i*/
  private val watches = new java.util.HashMap[Int, Int]

  /** watched(i) = j if scope(i) supports value j*/
  private val watched = new java.util.HashMap[Int, Int]

  def advise(problemState: ProblemState, event: Event, position: Int): Int = {

    val r = if (position == 0 && event <= Assignment) {
      arity
    } else {
      Option(watched.get(position)) match {
        case Some(w) if problemState.dom(scope(position)).present(w) => -1
        case _ => arity
      }

    }
    //println(s"Advising ${toString(problemState)}, $position, $watched: $r")
    r
  }
  def check(tuple: Array[Int]): Boolean = {
    tuple.tail.contains(tuple(0))
  }
  def init(ps: ProblemState): Outcome = ps
  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    def findNewSupport(i: Int) = {
      (1 until arity).find(j => ps.dom(scope(j)).present(i)) match {
        case Some(s) =>
          watches.put(i, s)
          watched.put(s, i)
          true
        case None => false
      }
    }

    ps.filterDom(variable) { i =>
      Option(watches.get(i))
        .map { j =>
          ps.dom(scope(j)).present(i) || {
            watched.remove(j)
            findNewSupport(i)
          }
        }
        .getOrElse(findNewSupport(i))

    }.andThen { ps =>
      val dom = ps.dom(variable)
      val find = Iterator.range(1, arity).filterNot(i => ps.dom(scope(i)).disjoint(dom))
      assert(find.hasNext)
      val first = find.next()
      if (find.hasNext) {
        ps
      } else {
        ps.intersectDom(scope(first), dom)
      }

    }.entailIf(this, { ps =>
      val dom = ps.dom(variable)
      dom.isAssigned && (1 until arity).exists { i =>
        val d = ps.dom(scope(i))
        d.isAssigned && dom.head == d.head
      }
    })

  }
  def simpleEvaluation: Int = 2

}