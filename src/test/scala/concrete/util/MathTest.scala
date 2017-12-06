package concrete.util

import org.scalatest.{FlatSpec, Matchers}
import Math.partialOrderingVector

class MathTest extends FlatSpec with Matchers {

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


}
