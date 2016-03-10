package concrete

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import cspom.util.BitVector

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
}