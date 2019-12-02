package concrete.util

import Math.partialOrderingVector
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MathTest extends AnyFlatSpec with Matchers {

  "ParetoMax algorithm" should "correctly extract maximum vectors" in {
    val v = Seq(
      Seq(-1, -2),
      Seq(-2, -1),
      Seq(-2, -2),
      Seq(-3, -5),
      Seq(-2, -6),
      Seq(-1, -7),
      Seq(-4, -3),
      Seq(-3, -4),
      Seq(-3, -6)
    )

    Math.paretoMin(v) shouldBe Seq(Seq(-1, -7), Seq(-4, -3), Seq(-3, -6))

  }

  "ceilDiv" should "correctly compute" in {
    Math.ceilDiv(-12, 35) shouldBe 0
  }


}
