package concrete
package heuristic
package value

import scala.util.Random

final class RandomValue(pm: ParameterManager) extends ValueHeuristic {

  override def toString = "random"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  private val rand = {
    val seed = pm.getOrElse("heuristic.value.seed", 0L) + pm.getOrElse("iteration", 0)
    new Random(seed)
  }

  override def selectIndex(variable: Variable, dom: Domain): Int = {
    @annotation.tailrec
    def apply(i: Int, v: Int): Int = if (i <= 0) { v } else { apply(i - 1, dom.next(v)) }

    apply(rand.nextInt(dom.size), dom.head)
  }
  def shouldRestart = true
}
