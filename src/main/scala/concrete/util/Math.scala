package concrete.util

import scala.util.Random
import scala.collection.mutable.HashSet

object Math {
  /**
   * Returns the product of {@code a} and {@code b}, provided it does not overflow.
   *
   * @throws ArithmeticException if {@code a * b} overflows in signed {@code int} arithmetic
   */
  def checkedMultiply(a: Int, b: Int): Int = {
    val result = a.toLong * b;
    if (result != result.toInt) throw new ArithmeticException("overflow")
    result.toInt;
  }

  /**
   * Returns the sum of {@code a} and {@code b}, provided it does not overflow.
   *
   * @throws ArithmeticException if {@code a + b} overflows in signed {@code int} arithmetic
   */
  def checkedAdd(a: Int, b: Int): Int = {
    val result = a.toLong + b;
    if (result != result.toInt) throw new ArithmeticException("overflow")
    result.toInt;
  }

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

  def floorDiv(dividend: Int, divisor: Int): Int = {
    val roundedTowardsZeroQuotient = dividend / divisor;
    val dividedEvenly = (dividend % divisor) == 0;
    if (dividedEvenly) {
      roundedTowardsZeroQuotient;
    } else {
      // If they're of opposite sign then we rounded 
      // UP towards zero so we rem one. If they're of the same sign then 
      // we rounded DOWN towards zero, so we are done.

      if (divisor.signum == dividend.signum) {
        roundedTowardsZeroQuotient;
      } else {
        roundedTowardsZeroQuotient - 1;
      }
    }
  }

  def ceilDiv(dividend: Int, divisor: Int): Int = {

    val roundedTowardsZeroQuotient = dividend / divisor;
    val dividedEvenly = (dividend % divisor) == 0;
    if (dividedEvenly) {
      roundedTowardsZeroQuotient;
    } else {
      // If they're of opposite sign then we rounded 
      // UP towards zero so we're done. If they're of the same sign then 
      // we rounded DOWN towards zero, so we need to add one.

      if (divisor.signum == dividend.signum) {
        roundedTowardsZeroQuotient + 1;
      } else {
        roundedTowardsZeroQuotient;
      }
    }
  }

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
    var s = new HashSet[Int]
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
}