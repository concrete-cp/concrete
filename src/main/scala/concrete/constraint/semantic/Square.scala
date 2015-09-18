package concrete.constraint.semantic;

import scala.IndexedSeq
import concrete.Domain
import concrete.Variable
import concrete.constraint.BCCompanion
import concrete.constraint.Constraint
import concrete.constraint.BC
import concrete.constraint.Removals
import concrete.Contradiction
import concrete.ProblemState
import concrete.Outcome

final class SquareBC(val x: Variable, val y: Variable)
    extends Constraint(Array(x, y)) with BC {

  def init(ps: ProblemState) = ps
  //  val corresponding = Array(
  //    x.dom.allValues map { v => y.dom.index(a * v + b) },
  //    y.dom.allValues map { v =>
  //      val r = v - b
  //      if (r % a == 0) x.dom.index(r / a) else -1
  //    })

  def check(t: Array[Int]) = t(0) == t(1) * t(1)

  def shave(ps: ProblemState) = {
    ps.shaveDom(x, ps.dom(y).span.sq)
      .shaveDom(y, ps.dom(x).span.sqrt)
  }

  override def toString(ps: ProblemState) = s"${x.toString(ps)} =BC= ${y.toString(ps)}²"

  def advise(ps: ProblemState, pos: Int) = 3 // else (x.dom.size + y.dom.size)
  val simpleEvaluation = 2
}

/**
 * Constraint x = y².
 *
 * @param x
 * @param y
 */
final class SquareAC(val x: Variable, val y: Variable)
    extends Constraint(Array(x, y)) with Removals with BCCompanion {
  def init(ps: ProblemState) = ps

  def skipIntervals = false
  //  val corresponding = Array(
  //    x.dom.allValues map { v => y.dom.index(a * v + b) },
  //    y.dom.allValues map { v =>
  //      val r = v - b
  //      if (r % a == 0) x.dom.index(r / a) else -1
  //    })

  def check(t: Array[Int]) = t(0) == t(1) * t(1)

  private def consistentX(xValue: Int, yDomain: Domain) = {
    Square.sqrt(xValue).exists { root =>
      yDomain.present(root) || yDomain.present(-root)
    }

  }

  private def consistentY(yValue: Int, xDomain: Domain) = {
    math.abs(yValue) < Square.MAX_SQUARE && xDomain.present(yValue * yValue)
  }

  def revise(ps: ProblemState, modified: Seq[Int]): Outcome = {
    val s = skip(modified)

    val ps0 = if (s == 0) { ps } else {
      ps.filterDom(x)(v => consistentX(v, ps.dom(y)))
    }

    val ps1 = if (s == 1) { ps0 } else {
      ps0.filterDom(y)(v => consistentY(v, ps0.dom(x)))
    }

    ps1.entailIfFree(this)
  }

  override def isConsistent(ps: ProblemState) =
    if (ps.dom(x).exists(v => consistentX(v, ps.dom(y))) && ps.dom(y).exists(v => consistentY(v, ps.dom(x))))
      ps
    else
      Contradiction

  override def toString(ps: ProblemState) = s"${x.toString(ps)} =AC= ${y.toString(ps)}²"

  def getEvaluation(ps: ProblemState) = ps.dom(x).size + ps.dom(y).size
  val simpleEvaluation = 2
}

object Square {
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

  val MAX_SQUARE = 46340 // sqrt(Int.MaxValue)
}
