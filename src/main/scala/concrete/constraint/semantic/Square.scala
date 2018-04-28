package concrete
package constraint
package semantic

import bitvectors.BitVector
import concrete.util.Interval

final class SquareBC(val x: Variable, val y: Variable)
  extends Constraint(Array(x, y)) with BC with ItvArrayFixPoint {

  val simpleEvaluation = 2

  //  val corresponding = Array(
  //    x.dom.allValues map { v => y.dom.index(a * v + b) },
  //    y.dom.allValues map { v =>
  //      val r = v - b
  //      if (r % a == 0) x.dom.index(r / a) else -1
  //    })

  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = t(0) == t(1) * t(1)

  private def reviseX(itv: Array[Domain]): Option[Interval] = Some(itv(1).span.sq)
  private def reviseY(itv: Array[Domain]): Option[Interval] = Some(itv(0).span.sqrt)

  val ops = Array(reviseX(_), reviseY(_))

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {
    fixPoint(ps)
  }

  override def toString(ps: ProblemState) = s"${x.toString(ps)} =BC= ${y.toString(ps)}²"

  def advise(ps: ProblemState, pos: Int) = 3 // else (x.dom.size + y.dom.size)
}

/**
  * Constraint x = y².
  *
  * @param x
  * @param y
  */
final class SquareAC(val x: Variable, val y: Variable)
  extends Constraint(Array(x, y)) with BCCompanion {
  val simpleEvaluation = 2

  def init(ps: ProblemState): Outcome = ps.removeUntil(x, 0)

  //  val corresponding = Array(
  //    x.dom.allValues map { v => y.dom.index(a * v + b) },
  //    y.dom.allValues map { v =>
  //      val r = v - b
  //      if (r % a == 0) x.dom.index(r / a) else -1
  //    })

  def skipIntervals = false

  def check(t: Array[Int]): Boolean = t(0) == t(1) * t(1)

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val domY = ps.dom(y)
    ps.filterDom(x)(v => consistentX(v, domY))
      .andThen { ps0 =>
        val domX = ps0.dom(x)
        ps0.filterDom(y)(v => consistentY(v, domX))
      }
      .entailIfFree(this)
  }

  private def consistentX(xValue: Int, yDomain: Domain) = {
    Square.sqrt(xValue).exists { root =>
      yDomain(root) || yDomain(-root)
    }
  }

  private def consistentY(yValue: Int, xDomain: Domain) = {
    math.abs(yValue) < Square.MAX_SQUARE && xDomain(yValue * yValue)
  }

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome = {
    val domY = ps.dom(y)
    val domX = ps.dom(x)
    if (domX.exists(v => consistentX(v, domY)) && domY.exists(v => consistentY(v, domX))) {
      ps
    } else {
      Contradiction(scope)
    }
  }

  override def toString(ps: ProblemState) = s"${x.toString(ps)} =AC= ${y.toString(ps)}²"

  def advise(ps: ProblemState, event: Event, pos: Int): Int = {
    val e = ps.card(x) + ps.card(y)

    if (skip(ps, e)) {
      -2
    } else {
      e
    }
  }
}

object Square {
  val MAX_SQUARE = 46340 // sqrt(Int.MaxValue)

  def sqrt(x: Int): Option[Int] = {
    if ((x & 2) == 2 || (x & 7) == 5) {
      None
    } else if ((x & 11) == 8 || (x & 31) == 20) {
      None
    } else if ((x & 47) == 32 || (x & 127) == 80) {
      None
    } else if ((x & 191) == 128 || (x & 511) == 320) {
      None
    } else {
      val root = Math.sqrt(x).toInt
      if (root * root == x) {
        Some(root)
      } else {
        None
      }

    }
  }
}
