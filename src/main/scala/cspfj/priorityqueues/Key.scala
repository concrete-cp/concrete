package cspfj.priorityqueues
import scala.annotation.tailrec
import cspfj.problem.Variable

object Key {

  def prod(num: Array[Variable]) = {
    @tailrec
    def p(i: Int, r: Int): Int = {
      if (i >= 0) {
        val v = num(i).dom.size
        if (r > Int.MaxValue / v) Int.MaxValue
        else p(i - 1, r * v)
      } else r
    }

    p(num.length - 1, 1)
  }

}

trait Key[T] extends Ordering[T] {
  def getKey(o: T): Int

  def compare(x: T, y: T) = getKey(x).compareTo(getKey(y))
}

