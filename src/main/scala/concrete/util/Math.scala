package concrete.util

import scala.collection.mutable
import scala.math.BigDecimal.RoundingMode
import scala.math.Ordering.Double.TotalOrdering
import scala.util.Random

object Math {

  private val LOG2 = math.log(2.0)


  def floorDiv(var0: Long, var2: Long): Long = {
    var var4 = var0 / var2
    if ((var0 ^ var2) < 0L && var4 * var2 != var0) var4 -= 1
    var4
  }

  def floorDiv(var0: Int, var1: Int): Int = {
    var var2 = var0 / var1
    if ((var0 ^ var1) < 0 && var2 * var1 != var0) var2 -= 1
    var2
  }

  def ceilDiv(x: Int, y: Int): Int = -floorDiv(-x, y)
  def ceilDiv(x: Long, y: Long): Long = -floorDiv(-x, y)

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

  def even(a: Int): Boolean = (a & 0x1) == 0

  /**
    * Robert Floyd algorithm to pick M elements from 1..N
    *
    * initialize set S to empty
    * for J := N-M + 1 to N do
    * T := RandInt(1, J)
    * if T is not in S then
    * insert T in S
    * else
    * insert J in S
    *
    * Note : slightly modified to pick M elements from 0..N-1
    */
  def randSet(m: Int, n: Int, rand: Random): Set[Int] = {
    val s = new mutable.HashSet[Int]
    for (j <- (n - m) until n) {
      val t = rand.nextInt(j + 1)
      if (s(t)) {
        s += j
      } else {
        s += t
      }
    }
    s.toSet
  }

  def any2Int(v: Any): Int = {
    v match {
      case v: Int => v
      case v: BigInt if v.isValidInt => v.intValue
      case v: Long if v.isValidInt => v.toInt
      case true => 1
      case false => 0
      case v: Any => throw new AssertionError(s"value $v cannot be handled")
    }

  }

  /**
    * Same as Math.exp() but returns a BigInteger. Oriented towards big numbers.
    * Works even for outputs that exceed the double range
    *
    * @param b Should be a large positive value
    * @return The value of e (base of the natural logarithms) raised to the power b
    */
  def bigexp(b: Double): BigInt = {
    require(!b.isNaN && !b.isInfinite, "Infinite or negative values not accepted: " + b)
    // e^b = e^(b2+c) = e^b2 2^t with e^c = 2^t
    val bc = 680.0
    if (b < bc) {
      BigDecimal.valueOf(math.exp(b)).setScale(0, RoundingMode.HALF_EVEN).toBigInt
    }
    else {
      val t = math.ceil((b - bc) / LOG2).toInt
      val b2 = b - t * LOG2
      val v = BigDecimal.valueOf(math.exp(b2)).setScale(0, RoundingMode.HALF_EVEN).toBigInt
      v << t
    }
  }


  /**
    * Computes the natural logarithm of a BigInteger. Works for really big
    * integers (practically unlimited)
    *
    * @param value Argument, positive integer
    * @return Natural logarithm, as in <tt>Math.log()</tt>
    */
  def logBigInteger(value: BigInt): Double = {
    var v = value
    val blex = v.bitLength - 1022; // any value in 60..1023 is ok
    if (blex > 0) v >>= blex
    val res = math.log(v.doubleValue)
    if (blex > 0) res + blex * LOG2 else res
  }

  def logSumOfExponentials(xs: Seq[Double]): Double = {
    if (xs.isEmpty) 0
    else {
      val max = xs.max(TotalOrdering)
      val sum = xs.filterNot(_.isNegInfinity).map(x => math.exp(x - max)).sum
      max + math.log(sum)
    }
  }

  def paretoMin[T](uv: Seq[T])(implicit ordering: PartialOrdering[T]): Seq[T] = {
    uv.filterNot { v => uv.exists(w => ordering.gt(v, w)) }
  }

  implicit def partialOrderingVector[T](implicit ordering: Ordering[T]): PartialOrdering[Seq[T]] =
    new PartialOrdering[Seq[T]] {
      def tryCompare(x: Seq[T], y: Seq[T]): Option[Int] = ???

      def lteq(is: Seq[T], js: Seq[T]): Boolean =
        (is lazyZip js).forall { case (i, j) => ordering.lteq(i, j) }
    }

  def logNormalLong(rand: Random, mean: Double, stdev: Double): Long = {
    math.round(logNormal(rand, mean, stdev))
  }

  def logNormal(rand: Random, mean: Double, stdev: Double): Double = {
    val variance = stdev * stdev
    val m2 = mean * mean
    val m = math.log(m2 / math.sqrt(variance + m2))
    val v = math.sqrt(math.log(1 + variance / m2))
    math.exp(m + v * rand.nextGaussian())
  }

//  def main(args: Array[String]): Unit = {
//    val r = new Random(0)
//    for (i <- 0 until 50) {
//      println((0 until 10).toSet -- randSet(9, 10, r))
//    }
//  }
}