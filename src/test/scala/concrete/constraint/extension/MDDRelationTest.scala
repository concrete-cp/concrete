package concrete.constraint.extension

import scala.collection.SortedSet
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import concrete.IntDomain
import concrete.Variable
import org.scalatest.Inspectors
import org.scalatest.OptionValues

class MDDRelationTest extends FlatSpec with Matchers with PropertyChecks with OptionValues {

  val mdd = new MDDRelation(MDD0 + Array(1, 2, 3) + Array(1, 3, 4) + Array(1, 2, 5) + Array(2, 3, 5))

  it should "have correct sizes" in {
    mdd should have size 4
    mdd.edges shouldBe 9
    mdd.lambda shouldBe BigInt(4)
  }

  it should "find supports" in {
    val v0 = new Variable("V0", IntDomain(0 to 2))
    val v1 = new Variable("V1", IntDomain(0 to 3))
    val v2 = new Variable("V2", IntDomain(0 to 5))

    val scope = Array(v0.initDomain, v1.initDomain, v2.initDomain)

    mdd.findSupport(scope, 0, 0) shouldBe None
    mdd.findSupport(scope, 0, 1) should contain oneOf (Array(1, 2, 3), Array(1, 3, 4), Array(1, 2, 5))
    mdd.findSupport(scope, 0, 2).value shouldBe Array(2, 3, 5)
    mdd.findSupport(scope, 1, 0) shouldBe None
    mdd.findSupport(scope, 1, 1) shouldBe None
    mdd.findSupport(scope, 1, 2) should contain oneOf (Array(1, 2, 3), Array(1, 2, 5))
    mdd.findSupport(scope, 1, 3) should contain oneOf (Array(1, 3, 4), Array(2, 3, 5))
    mdd.findSupport(scope, 2, 0) shouldBe None
    mdd.findSupport(scope, 2, 1) shouldBe None
    mdd.findSupport(scope, 2, 2) shouldBe None
    mdd.findSupport(scope, 2, 3).value shouldBe Array(1, 2, 3)
    mdd.findSupport(scope, 2, 4).value shouldBe Array(1, 3, 4)
    mdd.findSupport(scope, 2, 5) should contain oneOf (Array(1, 2, 5), Array(2, 3, 5))

    scope(1) = scope(1).remove(2)

    mdd.findSupport(scope, 0, 0) shouldBe None
    mdd.findSupport(scope, 0, 1).value shouldBe Array(1, 3, 4)
    mdd.findSupport(scope, 0, 2).value shouldBe Array(2, 3, 5)
    mdd.findSupport(scope, 1, 0) shouldBe None
    mdd.findSupport(scope, 1, 1) shouldBe None
    mdd.findSupport(scope, 1, 3) should contain oneOf (Array(1, 3, 4), Array(2, 3, 5))
    mdd.findSupport(scope, 2, 0) shouldBe None
    mdd.findSupport(scope, 2, 1) shouldBe None
    mdd.findSupport(scope, 2, 2) shouldBe None
    mdd.findSupport(scope, 2, 3) shouldBe None
    mdd.findSupport(scope, 2, 4).value shouldBe Array(1, 3, 4)
    mdd.findSupport(scope, 2, 5).value shouldBe Array(2, 3, 5)
  }

  it should "find single support" in {
    val v58 = new Variable("V58", IntDomain(0 to 160))
    val v59 = new Variable("V59", IntDomain(160))
    val v60 = new Variable("V60", IntDomain(0))

    val vars = IndexedSeq(v58, v59, v60)

    val scope = vars.map(_.initDomain)

    val mdd = MDDRelation(Seq(Array(160, 160, 0)))

    forAll(Gen.choose(0, 159)) { i =>
      mdd.findSupport(scope, 0, i) shouldBe None
    }

    mdd.findSupport(scope, 0, 158) shouldBe None
    mdd.findSupport(scope, 0, 160).value shouldBe Array(160, 160, 0)
  }
}
