package concrete.constraint.extension

import scala.util.Random
import cspom.extension.MDDNode
import scala.collection.mutable.HashMap

object MDDGenerator {
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

  /** max is inclusive */
  def randBigInt(max: BigInt, r: Random) = {
    require(max >= 0, max)
    if (max < Int.MaxValue) {
      BigInt(r.nextInt(max.toInt + 1))
    } else {
      var i: BigInt = null

      do {
        i = BigInt(max.bitLength, r) // r.nextInt(max.intValue)
      } while (i > max)

      i
    }
  }

  def tupleSplit(b: BigInt, d: Int, k: Int) = {
    val n = Array.ofDim[Int](k)
    var i = b
    var p = k - 1
    while (i > 0) {
      val (q, m) = i /% d
      n(p) = m.intValue
      i = q
      p -= 1
    }
    n
  }

  def apply(d: Int, k: Int, lambda: Int, rand: Random): MDD = {
    var data: concrete.constraint.extension.MDD = MDD0
    val n = BigInt(d).pow(k)

    for (j <- (n - lambda) until n) {
      val t = tupleSplit(randBigInt(j, rand), d, k)

      val newD = data.addTrie(t, 0)

      if (newD eq data) {
        data = data.addTrie(tupleSplit(j, d, k), 0)
      } else {
        data = newD
      }

    }

    data
  }

  def apply(d: Int, k: Int, l: Double, q: Double, rand: Random) = {
    val existing = Array.fill(k + 1)(new HashMap[cspom.extension.MDD[Int], cspom.extension.MDD[Int]]())
    generate(d, k, l, q, rand, existing)
  }

  private def generate(d: Int, k: Int, l: Double, q: Double, rand: Random,
                       existing: Array[HashMap[cspom.extension.MDD[Int], cspom.extension.MDD[Int]]]): cspom.extension.MDD[Int] = {
    if (k == 0) {
      if (rand.nextDouble < l) {
        cspom.extension.MDD.leaf
      } else {
        cspom.extension.MDD.empty
      }
    } else if (existing(k).nonEmpty && rand.nextDouble < q) {
      existing(k).keysIterator.drop(rand.nextInt(existing(k).size)).next
    } else {
      val t = Iterator.tabulate(d)(i =>
        i -> generate(d, k - 1, l, q, rand, existing))
        .filter(_._2.nonEmpty)

      val r =
        if (t.isEmpty) {
          cspom.extension.MDD.empty[Int]
        } else {
          new MDDNode(t.toMap)
        }

      existing(k).getOrElseUpdate(r, r)
    }
  }
}