package concrete
package heuristic
package value

import com.typesafe.scalalogging.LazyLogging

import scala.util.Random

final class RandomValue(rand: Random) extends ValueHeuristic with LazyLogging {

  override def toString = "random"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def selectIndex(variable: Variable, dom: Domain): Int = {
    @annotation.tailrec
    def apply(i: Int, v: Int): Int = if (i <= 0) { v } else { apply(i - 1, dom.next(v)) }
    val r = apply(rand.nextInt(dom.size), dom.head)
    logger.info(s"Random value is $r")
    r
  }

  def shouldRestart = true
}
