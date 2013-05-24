package concrete.heuristic.revision

import concrete.Variable
import scala.annotation.tailrec

object Key {

  def prod(num: Array[Variable]) = {
    @tailrec
    def p(i: Int, r: Int): Int =
      if (i < 0) r
      else {
        val v = num(i).dom.size
        if (r > Int.MaxValue / v) Int.MaxValue
        else p(i - 1, r * v)
      }

    p(num.length - 1, 1)
  }

}

trait Key[T] {
  def getKey(o: T, eval: Int): Int = getKey(o)
  
  def getKey(o: T): Int
}

