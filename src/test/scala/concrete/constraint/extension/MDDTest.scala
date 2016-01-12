package concrete.constraint.extension

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import scala.util.Random

final class MDDTest extends FlatSpec with Matchers with Inspectors {

  private val ts: MDD = MDD(Seq(Seq(0, 0), Seq(0, 1), Seq(1, 0)))

  val t = MDD0 + Array(1, 2, 3) + Array(1, 3, 4) + Array(1, 2, 5) + Array(2, 3, 5)
  val s = MDD0 + Array(1, 2, 5) + Array(1, 3, 4) + Array(1, 2, 3) + Array(2, 3, 5)

  val u = MDD(Seq(
    Seq(1, 2, 3),
    Seq(1, 3, 4),
    Seq(1, 2, 5),
    Seq(2, 3, 5)))

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

  it should "reduce" in {
    val m = MDD(Seq(
      Seq(2, 3, 2),
      Seq(1, 2, 1),
      Seq(1, 1, 1),
      Seq(1, 1, 3),
      Seq(3, 1, 1),
      Seq(3, 1, 3)))
      .reduce()

    m.lambda shouldBe BigInt(6)
    m.edges(1) shouldBe 11
   
  }
  
  it should "have same edges as CSPOM version" in {
    val mdd = MDDGenerator(5, 5, 1000, new Random(0)).reduce()
    
    val cspomMdd = cspom.extension.MDD(mdd).reduce
   
    mdd.lambda shouldBe cspomMdd.lambda
    
    mdd should contain theSameElementsAs cspomMdd
    
    mdd.edges(1) shouldBe cspomMdd.edges
  }
}
