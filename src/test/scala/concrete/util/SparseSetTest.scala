package concrete.util

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class SparseSetTest extends FlatSpec with Matchers {

  "SparseSet" should "detect containment" in {

    val map = new SparseSet(10) + 7

    val map2 = map + 6

    map2 should contain(7)
    map2 should contain(6)
    map2 should not contain (1)
    map should not contain (6)

  }
}