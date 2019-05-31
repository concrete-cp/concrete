package concrete.constraint.semantic

;

import bitvectors.BitVector
import concrete._
import concrete.constraint.Constraint

import scala.annotation.tailrec

object NeqVec {
  private val DISJ = -3
  private val NONE = -2
  private val TWOP = -1
}

final class NeqVec(x: Array[Variable], y: Array[Variable]) extends Constraint(x ++ y) {

  import NeqVec._

  require(x.length == y.length)

  val simpleEvaluation = 2

  private val size = arity / 2

  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = {
    t.view.splitAt(size).zipped.exists(_ != _)
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    singleFreeVariable(ps) match {
      case DISJ => ps.entail(this)
      case TWOP => ps
      case NONE => Contradiction(scope)
      case p if p < size =>
        ps.removeIfPresent(scope(p), ps.dom(scope(p + size)).head).entail(this)
      case p =>
        ps.removeIfPresent(scope(p), ps.dom(scope(p - size)).head).entail(this)
    }

  }

  override def toString(ps: ProblemState): String =
    x.map(_.toString(ps)).mkString("(", ", ", ")") + " /= " + y.map(_.toString(ps)).mkString("(", ", ", ")")

  def advise(ps: ProblemState, event: Event, p: Int): Int = arity //if (event <= Assignment) arity else -1

  @tailrec
  private def singleFreeVariable(ps: ProblemState,
                                 i: Int = 0,
                                 single: Int = NONE): Int = {
    if (i >= arity) {
      single
    } else {
      val dom = ps.dom(scope(i))
      if (dom.isAssigned) {
        val value = dom.head
        val dom2 = if (i < size) {
          ps.dom(scope(i + size))
        } else {
          ps.dom(scope(i - size))
        }
        if (dom2.contains(value)) {
          singleFreeVariable(ps, i + 1, single)
        } else {
          DISJ
        }
      } else if (single < 0) {
        singleFreeVariable(ps, i + 1, i)
      } else {
        TWOP
      }
    }

  }
}
