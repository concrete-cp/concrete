package concrete.heuristic.revision

import concrete.Variable
import scala.annotation.tailrec
import concrete.ProblemState

object Key {

  def prod(num: Array[Variable], s: ProblemState) = {
    @tailrec
    def p(i: Int, r: Int): Int =
      if (i < 0) r
      else {
        val v = s(num(i)).size
        if (r > Int.MaxValue / v) Int.MaxValue
        else p(i - 1, r * v)
      }

    p(num.length - 1, 1)
  }

}

trait Key[T] {
  def getKey(o: T, s: ProblemState, eval: Int): Int = getKey(o, s)

  def getKey(o: T, s: ProblemState): Int
}

