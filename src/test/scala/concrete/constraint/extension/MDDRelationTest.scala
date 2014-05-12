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

  def relation(arity: Int) = Gen.nonEmptyListOf(Gen.containerOfN[Array, Int](arity, Arbitrary.arbitrary[Int]))

  def domains = Gen.containerOfN[Array, SortedSet[Int]](5, Arbitrary.arbitrary[SortedSet[Int]].suchThat(_.nonEmpty))

  def pickValue(scope: Array[Variable]) = {
    for (
      p <- Gen.oneOf(scope.indices);
      i <- Gen.oneOf(scope(p).dom.indices.toSeq)
    ) yield {
      (p, i)
    }
  }

  "MDDs" should "work" in {

    forAll(domains) { doms =>
      val scope = doms.map(d => new Variable(null, IntDomain(d)))
      require(scope.size == 5)
      println(scope.mkString(" "))
      forAll(relation(5)) { relation =>
        val mdd = MDDRelation(relation)
        forAll(pickValue(scope)) {
          case (p, i) =>
            mdd.findSupport(scope, p, i, new Array(scope.size)) match {
              case Some(t) =>
                t(p) shouldBe i
                mdd should contain(t)
              case None =>
                Inspectors.forAll(mdd) { t: Array[Int] => t(p) should not be (i) }
            }
        }
      }

    }

  }

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

    val scope = Array(v0, v1, v2)
    val support = new Array[Int](scope.size)
    mdd.findSupport(scope, 0, 0, support) shouldBe None
    mdd.findSupport(scope, 0, 1, support) should contain oneOf (Array(1, 2, 3), Array(1, 3, 4), Array(1, 2, 5))
    mdd.findSupport(scope, 0, 2, support).value shouldBe Array(2, 3, 5)
    mdd.findSupport(scope, 1, 0, support) shouldBe None
    mdd.findSupport(scope, 1, 1, support) shouldBe None
    mdd.findSupport(scope, 1, 2, support) should contain oneOf (Array(1, 2, 3), Array(1, 2, 5))
    mdd.findSupport(scope, 1, 3, support) should contain oneOf (Array(1, 3, 4), Array(2, 3, 5))
    mdd.findSupport(scope, 2, 0, support) shouldBe None
    mdd.findSupport(scope, 2, 1, support) shouldBe None
    mdd.findSupport(scope, 2, 2, support) shouldBe None
    mdd.findSupport(scope, 2, 3, support).value shouldBe Array(1, 2, 3)
    mdd.findSupport(scope, 2, 4, support).value shouldBe Array(1, 3, 4)
    mdd.findSupport(scope, 2, 5, support) should contain oneOf (Array(1, 2, 5), Array(2, 3, 5))

    v1.dom.remove(2)
    mdd.findSupport(scope, 0, 0, support) shouldBe None
    mdd.findSupport(scope, 0, 1, support).value shouldBe Array(1, 3, 4)
    mdd.findSupport(scope, 0, 2, support).value shouldBe Array(2, 3, 5)
    mdd.findSupport(scope, 1, 0, support) shouldBe None
    mdd.findSupport(scope, 1, 1, support) shouldBe None
    mdd.findSupport(scope, 1, 3, support) should contain oneOf (Array(1, 3, 4), Array(2, 3, 5))
    mdd.findSupport(scope, 2, 0, support) shouldBe None
    mdd.findSupport(scope, 2, 1, support) shouldBe None
    mdd.findSupport(scope, 2, 2, support) shouldBe None
    mdd.findSupport(scope, 2, 3, support) shouldBe None
    mdd.findSupport(scope, 2, 4, support).value shouldBe Array(1, 3, 4)
    mdd.findSupport(scope, 2, 5, support).value shouldBe Array(2, 3, 5)
  }

  it should "find single support" in {
    val v58 = new Variable("V58", IntDomain(0 to 160))
    val v59 = new Variable("V59", IntDomain(160))
    val v60 = new Variable("V60", IntDomain(0))

    val scope = Array(v58, v60, v59)
    val support = new Array[Int](scope.size)
    val mdd = MDDRelation(Seq(Array(160, 0, 0)))

    forAll(Gen.choose(0, 159)) { i =>
      mdd.findSupport(scope, 0, i, support) shouldBe None
    }

    mdd.findSupport(scope, 0, 158, support) shouldBe None
    mdd.findSupport(scope, 0, 160, support).value shouldBe Array(160, 0, 0)
  }
}
