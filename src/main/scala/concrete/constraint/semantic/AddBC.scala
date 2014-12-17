package concrete.constraint.semantic;

import com.typesafe.scalalogging.LazyLogging
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.BC
import concrete.ProblemState

final class AddBC(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1)) with BC with LazyLogging {

  def check(t: Array[Int]) = t(0) == t(1) + t(2)

  def shave(ps: ProblemState) = {
    val result = ps.dom(this.result).span
    val v0 = ps.dom(this.v0).span
    val v1 = ps.dom(this.v1).span

    ps
      .shaveDom(this.result, v0 + v1)
      .shaveDom(this.v0, result - v1)
      .shaveDom(this.v1, result - v0)

  }

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} = ${v0.toString(ps)} + ${v1.toString(ps)}"

  def advise(domains: IndexedSeq[Domain], pos: Int) = 4

  def simpleEvaluation = 2
}
