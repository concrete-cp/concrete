package concrete
package constraint
package semantic;

import bitvectors.BitVector
import concrete.util.Interval

final class AbsBC(val result: Variable, val v0: Variable) extends Constraint(Array(result, v0)) with BC {
  //  val corresponding1 = result.dom.allValues map { v0.dom.index }
  //  val corresponding2 = result.dom.allValues map { v => v0.dom.index(-v) }
  //  val correspondingR = v0.dom.allValues map { v => result.dom.index(math.abs(v)) }

  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = t(0) == math.abs(t(1))

  private val shavers: Array[ProblemState => Outcome] = Array(
    { ps =>
      val dv0 = ps.dom(v0)
      ps.shaveDom(result, dv0.span.abs)
    },
    { ps: ProblemState =>

      val ri = ps.span(result)

      Interval.realUnion(ri, -ri) match {
        case Right((i, j)) =>
          val d0 = ps.dom(v0)
            .removeUntil(i.lb)
            .removeAfter(j.ub)
            .removeItv(i.ub + 1, j.lb - 1)

          ps.updateDom(v0, d0)
        case Left(i) => ps.shaveDom(v0, i)

      }

    })

  override def shave(state: ProblemState): Outcome = throw new IllegalStateException()

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {
    fixPointM(ps, shavers)
  }

  override def toString(ps: ProblemState) = s"${result.toString(ps)} =BC= |${v0.toString(ps)}|";

  def advise(ps: ProblemState, p: Int) = 6

  def simpleEvaluation = 1
}
