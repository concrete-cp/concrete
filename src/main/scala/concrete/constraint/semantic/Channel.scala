package concrete
package constraint
package semantic

import bitvectors.BitVector
import concrete.util.Boxed

/**
  * Constraint x(i) = j <=> x(j) = i
  */
final class Channel(x: Array[Variable], offset: Int) extends Constraint(x)
  with StatefulConstraint[Boxed[BitVector]] {

  def advise(problemState: ProblemState, event: Event, pos: Int): Int = {
    val fx = problemState(this)
    val card = fx.bv.cardinality
    card * card
  }

  def check(tuple: Array[Int]): Boolean = {
    x.indices.forall { i =>
      val j = tuple(i)
      tuple(j - offset) == i + offset
    }
  }

  def init(ps: ProblemState): Outcome =
    ps.fold(x) { (ps, v) =>
      ps.shaveDom(v, offset, offset + x.length - 1)
    }
      .updateState(this, Boxed(BitVector.filled(x.length)))

  def revise(problemState: ProblemState, mod: BitVector): Outcome = {
    //println(s"$mod of ${toString(problemState)}")
    // fx contains "free variables"
    var fx = problemState(this).bv

    // Check whether variables have been assigned
    problemState.fold(mod) { (ps, p) =>
      val dom = ps.dom(x(p))
      if (dom.isAssigned) {
        fx -= p

        val v = dom.singleValue

        ps.tryAssign(x(v - offset), p + offset)
      } else {
        ps
      }
    }
      .fold(fx) { (ps, p) =>
        ps.filterDom(x(p))(v => ps.dom(x(v - offset)).present(p + offset))
      }
      .updateState(this, Boxed(fx))

  }

  override def toString(ps: ProblemState): String = {
    s"channel $offset/${x.map(ps.dom(_).toString).mkString(", ")} fx = ${ps(this).bv}"
  }

  def simpleEvaluation: Int = 2
}