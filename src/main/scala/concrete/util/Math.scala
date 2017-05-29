package concrete.util

import scala.util.Random
import scala.collection.mutable.HashSet

object Math {

  def ceilDiv(x: Int, y: Int): Int = -java.lang.Math.floorDiv(-x, y)

  /** Make sure not to divide by some negative number as it would reverse the inequality **/
  def gcd(a: Seq[Int]): BigInt = a.map(BigInt(_)).reduce(_.gcd(_))

  def gcd(ia: Int, ib: Int): Int = {
    var d = 0
    var a = ia
    var b = ib
    while (even(a) && even(b)) {
      a /= 2
      b /= 2
      d += 1
    }
    while (a != b) {
      if (even(a)) {
        a /= 2
      } else if (even(b)) {
        b /= 2
      } else if (a > b) {
        a = (a - b) / 2
      } else {
        b = (b - a) / 2
      }
    }

    a * (0x1 << d)
  }

  def even(a: Int) = (a & 0x1) == 0

  /**
   * Robert Floyd algorithm to pick M elements from 1..N
   *
   * initialize set S to empty
   * for J := N-M + 1 to N do
   *   T := RandInt(1, J)
   *   if T is not in S then
   *     insert T in S
   *   else
   *     insert J in S
   */
  def randSet(m: Int, n: Int, rand: Random): Set[Int] = {
    val s = new HashSet[Int]
    for (j <- (n - m) until n) {
      val t = rand.nextInt(j)
      if (s(t)) {
        s += j
      } else {
        s += t
      }
    }
    s.toSet

  }

  def any2Int(v: Any) = {
    v match {
      case v: Int => v
      case v: Long if v.isValidInt => v.toInt
      case true => 1
      case false => 0
      case v: Any => throw new AssertionError(s"value $v cannot be handled")
    }

  }
}