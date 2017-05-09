package concrete.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

class SparseSetTest extends FlatSpec with Matchers with PropertyChecks {

  "SparseSet" should "detect containment" in {

    val map = new SparseSet(10) + 7

    val map2 = map + 6

    map2 should contain(7)
    map2 should contain(6)
    map2 should not contain (1)
    map should not contain (6)

  }

  it should "have a Set behavior, test case" in {
    val g = Seq((549, true), (1, true), (1, false), (549, false))

    var set = Set.empty[Int]
    var sparseSet = new SparseSet(math.max(g.size, 1001))

    for ((n, b) <- g) {
      if (b) {
        set += n
        sparseSet += n
      } else {
        set -= n
        sparseSet -= n
      }
      sparseSet should contain theSameElementsAs set
    }

  }

  it should "have a Set behavior" in {

    new SparseSet(10) + 0 should contain theSameElementsAs Seq(0)

    (new SparseSet(1000) + 154 + 995 - 154 + 995) should contain theSameElementsAs Set(995)

    forAll(Gen.nonEmptyListOf(Gen.zip(Gen.choose(0, 1000), Gen.oneOf(true, false)))) { g =>
      var set = Set.empty[Int]
      var sparseSet = new SparseSet(math.max(g.size, 1001))

      for ((n, b) <- g) {
        if (b) {
          set += n
          sparseSet += n
        } else {
          set -= n
          sparseSet -= n
        }
        sparseSet should contain theSameElementsAs set
      }
    }
  }
}