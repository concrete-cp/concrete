package concrete.util

import org.scalatest.{FlatSpec, Matchers}

class ScalaBitSetTest extends FlatSpec with Matchers {
  "ScalaBitSetTest" should "split" in {
    val sbs = new ScalaBitSet()
    sbs ++= 0 to 200

    sbs.rangeImpl(Some(5), Some(30)) should contain theSameElementsAs (5 until 30)
  }
}
