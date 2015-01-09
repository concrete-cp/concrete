package concrete.constraint.extension

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.Inspectors

final class MDDTest extends FlatSpec with Matchers with Inspectors {

  private val ts: MDD = MDD(Seq(Array(0, 0), Array(0, 1), Array(1, 0)))

  val t = MDD0 + Array(1, 2, 3) + Array(1, 3, 4) + Array(1, 2, 5) + Array(2, 3, 5)
  val s = MDD0 + Array(1, 2, 5) + Array(1, 3, 4) + Array(1, 2, 3) + Array(2, 3, 5)

  val u = MDD(Seq(
    Array(1, 2, 3),
    Array(1, 3, 4),
    Array(1, 2, 5),
    Array(2, 3, 5)))

  "MDD" should "detect containment" in {
    ts should contain(Array(0, 1))
    ts should not contain (Array(1, 1))

    t should contain(Array(1, 3, 4))
    t should not contain (Array(1, 2, 4))

    forAll(t.toSeq.map(_.toArray)) { tuple =>
      s should contain(tuple)
      u should contain(tuple)
    }
  }

  it should "iterate over all tuples" in {
    ts.iterator.size shouldBe ts.lambda
  }

  it should "compute its size correctly" in {
    ts.lambda shouldBe BigInt(3)

    t.lambda shouldBe s.lambda

    u.lambda shouldBe t.lambda
  }

}
