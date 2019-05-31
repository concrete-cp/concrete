package concrete.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SparseSetBufferTest extends FlatSpec with Matchers with ScalaCheckPropertyChecks {

  "SparseSetBuffer" should "detect containment" in {

    val map = new SparseSetBuffer() + 7

    val map2 = map + 6

    map2 should contain(7)
    map2 should contain(6)
    map2 should not contain 1
    map should not contain 6

  }

  it should "be empty by default" in {
    var s = new SparseSetBuffer()
    s should not contain 0

    s += 3
    s should not contain 0

  }

  it should "have a Set behavior, test case" in {
    val g = Seq((549, true), (1, true), (1, false), (549, false))

    var set = Set.empty[Int]
    var SparseSetBuffer = new SparseSetBuffer()

    for ((n, b) <- g) {
      if (b) {
        set += n
        SparseSetBuffer += n
      } else {
        set -= n
        SparseSetBuffer -= n
      }
      SparseSetBuffer should contain theSameElementsAs set
    }

  }

  it should "have a Set behavior" in {

    new SparseSetBuffer() + 0 should contain theSameElementsAs Seq(0)

    (new SparseSetBuffer() + 154 + 995 - 154 + 995) should contain theSameElementsAs Set(995)

    forAll(Gen.nonEmptyListOf(Gen.zip(Gen.choose(0, 1000), Gen.oneOf(true, false)))) { g =>
      var set = Set.empty[Int]
      var SparseSetBuffer = new SparseSetBuffer()

      for ((n, b) <- g) {
        if (b) {
          set += n
          SparseSetBuffer += n
        } else {
          set -= n
          SparseSetBuffer -= n
        }
        SparseSetBuffer should contain theSameElementsAs set
      }
    }
  }
}