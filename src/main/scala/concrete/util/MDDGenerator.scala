package concrete.util

import scala.util.Random
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import concrete.constraint.extension.MDD0
import concrete.constraint.extension.MDD
import concrete.constraint.extension.MDDLeaf

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

  def giveStructure(mdd: MDD, q: Double, rand: Random, ts: Int) = {
    val existing = new HashMap[Int, ArrayBuffer[MDD]]()

    def giveStruct(n: MDD, k: Int): MDD = {
      if (n eq MDDLeaf) {
        n
      } else n.cache(ts) {
        val e = existing.getOrElseUpdate(k, new ArrayBuffer())
        if (e.nonEmpty && rand.nextDouble() < q) {
          e(rand.nextInt(e.size))
        } else {
          val newMDD = MDD(
            n.traverseST
              .map {
                case (i, m) => i -> giveStruct(m, k + 1)
              }
              .toMap)

          val r = if (newMDD == n) n else newMDD
          e += r
          r
        }

      }
    }

    giveStruct(mdd, 0)
  }

}