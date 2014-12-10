package concrete.constraint.semantic;

import scala.annotation.tailrec
import concrete.Domain
import concrete.Revised
import concrete.Variable
import concrete.constraint.Constraint

import scala.collection.SeqView

final class NeqVec(x: Array[Variable], y: Array[Variable]) extends Constraint(x ++ y) {
  type State = Unit
  def initState = Unit
  require(x.length == y.length)

  val zip = x.zip(y)

  val size = arity / 2

  def check(t: Array[Int]) = {
    t.view.splitAt(size).zipped.exists(_ != _)
  }

  def revise(domains: IndexedSeq[Domain], s: State) = {
    val p = singleFreeVariable(domains)

    if (p < 0) {
      Revised(domains)
    } else if (p < size) {
      Revised(domains.updated(p, domains(p).remove(domains(p + size).head)))
    } else {
      Revised(domains.updated(p, domains(p).remove(domains(p - size).head)))
    }
  }

  @tailrec
  private def singleFreeVariable(
    in: IndexedSeq[Domain],
    i: Int = 0,
    single: Int = -1): Int = {

    if (i >= arity) {
      single
    } else if (in(i).size > 1) {
      if (single < 0) {
        singleFreeVariable(in, i + 1, i)
      } else {
        -1
      }
    } else {
      singleFreeVariable(in, i + 1, single)
    }
  }

  override def toString(domains: IndexedSeq[Domain], s: State) = domains.take(size).mkString("(", ", ", ")") + " /= " + domains.drop(size).mkString("(", ", ", ")")

  def advise(domains: IndexedSeq[Domain], p: Int) = arity

  val simpleEvaluation = 2
}
