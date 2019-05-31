package concrete
package constraint
package semantic

import bitvectors.BitVector

/**
  * v0 - v1 != c
  *
  * or
  *
  * v0 != v1 + c
  */
final class Neq(v0: Variable, v1: Variable, c: Int = 0) extends Constraint(Array(v0, v1)) {
  val simpleEvaluation = 2

  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = t(0) - t(1) != c

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val d0 = ps.dom(v0)
    if (d0.isAssigned) {
      ps.removeIfPresent(v1, d0.head - c).entail(this)
    } else {
      val d1 = ps.dom(v1)

      if (d1.isAssigned) {
        val v = d1.head + c
        if (d0.contains(v)) {
          ps.updateDom(v0, d0 - v).entail(this)
        } else {
          ps.entail(this)
        }

      } else if (!d0.span.intersects(d1.span + c)) {
        ps.entail(this)
      } else {
        ps
      }
    }
  }

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome = {
    val v0dom = ps.dom(v0)
    val r = !v0dom.isAssigned || {
      val v1dom = ps.dom(v1)
      !v1dom.isAssigned || (v0dom.head - v1dom.head != c)
    }
    if (r) ps else Contradiction(scope)
  }

  override def toString(ps: ProblemState) = s"${v0.toString(ps)} /= ${v1.toString(ps)}${
    if (c > 0) s" + $c"
    else if (c < 0) s" - ${-c}"
    else ""
  }"

  def advise(ps: ProblemState, event: Event, p: Int): Int = {
    // Entailment can be detected even if event is not an assigment
    //if (event <= Assignment) 2 else -1
    2
  }
}

class NeqC(x: Variable, y: Int) extends Constraint(x) {
  def advise(problemState: ProblemState, event: Event, pos: Int): Int = 2

  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = t(0) != y

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome = {
    val d = ps.dom(x)
    if (!d.isAssigned || d.singleValue != y) ps else Contradiction(scope)
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    ps.removeIfPresent(x, y).entail(this)
  }

  def simpleEvaluation: Int = 1

  override def toString(ps: ProblemState) = s"${x.toString(ps)} /= $y"
}


final class NeqReif(val r: Variable, val x: Variable, val y: Variable) extends Constraint(Array(r, x, y)) {
  def advise(problemState: ProblemState, event: Event, pos: Int): Int = 3

  def check(tuple: Array[Int]): Boolean = tuple(0) == (if (tuple(1) == tuple(2)) 0 else 1)

  def init(ps: ProblemState): Outcome = ps

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val dx = ps.dom(x)
    val dy = ps.dom(y)
    ps.dom(r) match {
      case BooleanDomain.UNKNOWNBoolean =>
        if (dx disjoint dy) {
          ps.updateDomNonEmpty(r, BooleanDomain.TRUE).entail(this)
        } else if (dx.isAssigned && dy.isAssigned) {
          // Necessarily grounded to the same value since not disjoint
          ps.updateDomNonEmpty(r, BooleanDomain.FALSE).entail(this)
        } else {
          ps
        }

      case BooleanDomain.FALSE =>
        val d = dx & dy
        ps.updateDom(x, d)
          .andThen { ps =>
            if (d.size < dy.size) ps.updateDom(y, d) else ps
          }

      case BooleanDomain.TRUE =>
        if (dx.isAssigned) {
          ps.removeIfPresent(y, dx.head).entail(this)
        } else if (dy.isAssigned) {
          ps.removeIfPresent(x, dy.head).entail(this)
        } else if (dx disjoint dy) {
          ps.entail(this)
        } else {
          ps
        }
    }
  }

  def simpleEvaluation: Int = 1
}