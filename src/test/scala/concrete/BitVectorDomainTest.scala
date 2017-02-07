package concrete

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import bitvectors.BitVector

class BitVectorDomainTest extends FlatSpec with Matchers {

  "BitVectorDomains" should "shift automatically" in {
    val bv = IntDomain.ofBitVector(0, BitVector((100 until 150) ++ (200 until 250)), 100).asInstanceOf[BitVectorDomain]

    bv.bitVector.nextSetBit(0) shouldBe 0
    bv.size shouldBe 100
    bv.head shouldBe 100
    bv.last shouldBe 249

  }

  it should "reuse previous instance" in {
    val bv = IntDomain.ofSeq(0, 3, 6).filter(_ > 0)
    bv & (3, 6) should be theSameInstanceAs bv
  }

  it should "shave" in {
    val bv = IntDomain.ofBitVector(0, BitVector(Seq(0, 10, 50, 70, 100)), 5)
    bv.removeTo(10).view should contain theSameElementsAs Seq(50, 70, 100)

    bv.removeTo(100) shouldBe 'empty

    val bv2 = IntDomain.ofBitVector(10, BitVector(Seq(0, 10, 50, 70, 100)), 5)
    bv2.removeTo(20).view should contain theSameElementsAs Seq(60, 80, 110)

    val bv3 = IntDomain.ofBitVector(10, BitVector(Seq(0, 10, 20, 30)), 4)
    println(bv3)
    val cleared = bv3.removeTo(20)
    cleared.view should contain theSameElementsAs Seq(30, 40)

    val bv4 = IntDomain.ofBitVector(1, BitVector(Seq(1, 3)), 2)
    bv4.removeUntil(4).view should contain theSameElementsAs Seq(4)
  }
}