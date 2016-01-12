package concrete.util

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class ArraySetTest extends FlatSpec with Matchers {
  "ArraySet" should "use correct hashes" in {
    val as = ArraySet.empty[Int] + Array(0, 1)

    assert(as(Array(0, 1)))

    val set = Set.empty[Array[Int]] + Array(0, 1)
    assert(!set(Array(0, 1)))

  }
}