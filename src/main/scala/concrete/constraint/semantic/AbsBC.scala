package concrete
package constraint
package semantic;

import concrete.util.Interval

final class AbsBC(val result: Variable, val v0: Variable) extends Constraint(Array(result, v0)) with BC {
  //  val corresponding1 = result.dom.allValues map { v0.dom.index }
  //  val corresponding2 = result.dom.allValues map { v => v0.dom.index(-v) }
  //  val correspondingR = v0.dom.allValues map { v => result.dom.index(math.abs(v)) }

  def init(ps: ProblemState) = ps

  def check(t: Array[Int]) = t(0) == math.abs(t(1))

  override def shave(ps: ProblemState): Outcome = {

    ps.shaveDom(result, ps.span(v0).abs)
      .andThen { ps =>

        val ri = ps.span(result)
        val v0span = ps.span(v0)

        Interval.realUnion(v0span intersect ri, v0span intersect -ri) match {
          case Some(Right((i, j))) =>
            val d0 = ps.dom(v0)
              .removeUntil(i.lb)
              .removeAfter(j.ub)
              .removeItv(i.ub + 1, j.lb - 1)

            ps.updateDom(v0, d0)
          case Some(Left(i)) => ps.shaveDom(v0, i)

          case None => Contradiction
        }

      }
  }

  override def toString(ps: ProblemState) = s"${result.toString(ps)} =BC= |${v0.toString(ps)}|";

  def advise(ps: ProblemState, p: Int) = 6

  def simpleEvaluation = 1
}
