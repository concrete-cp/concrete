package concrete.util;

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class SmallBitVectorTest extends FlatSpec with Matchers {

  "SmallBitVector" should "be filled" in {
    val bitVector = BitVector.filled(50)
    assert(bitVector(49));
    assert(bitVector(32));
    assert(!bitVector(50));
    bitVector.cardinality shouldBe 50

    BitVector.cleared(50).nextSetBit(0) shouldBe -1
  }

  it should "set bits" in {
    var bitVector = BitVector.cleared(50)
    assert(!bitVector(30))
    bitVector += 30
    assert(bitVector(30))
    bitVector -= 30
    assert(!bitVector(30))
    bitVector = bitVector.set(30, true)
    assert(bitVector(30))
  }

  it should "get bits" in {
    val bitVector = BitVector.cleared(50) + 46
    assert(!bitVector(0));
    assert(!bitVector(45));
    assert(bitVector(46));
  }

  it should "compute next bits" in {
    val bitVector = BitVector.cleared(50) + 46
    bitVector.nextSetBit(0) shouldBe 46
    bitVector.nextSetBit(46) shouldBe 46
    bitVector.nextSetBit(47) shouldBe -1

    val bv2 = bitVector + 49
    bv2.nextSetBit(47) shouldBe 49
  }

  it should "compute prev clear bits" in {
    val bitVector = BitVector.filled(50) - 46 - 48
    bitVector.prevClearBit(47) shouldBe 46
    bitVector.prevClearBit(46) shouldBe -1
    bitVector.prevClearBit(45) shouldBe -1

    bitVector.prevClearBit(50) shouldBe 48
    bitVector.prevClearBit(49) shouldBe 48
  }

  it should "compute prev set bits" in {
    val bitVector = BitVector.cleared(50) + 26 + 29

    bitVector.prevSetBit(27) shouldBe 26
    bitVector.prevSetBit(26) shouldBe -1
    bitVector.prevSetBit(25) shouldBe -1

    bitVector.prevSetBit(51) shouldBe 29
    bitVector.prevSetBit(50) shouldBe 29
    bitVector.prevSetBit(49) shouldBe 29

    val bv2 = bitVector + 49
    bv2.prevSetBit(65) shouldBe 49
    bv2.prevSetBit(63) shouldBe 49
  }

  it should "convert to String" in {
    (BitVector.cleared(50) + 46 + 49).toString shouldBe "{46, 49}"
  }

  it should "clear bits from" in {
    val bitVector = BitVector.cleared(50) + 26 + 29 + 30

    val bv2 = bitVector.clearFrom(27)

    bv2 should not be theSameInstanceAs(bitVector)

    bv2.cardinality shouldBe 1

    assert(bv2(26))
    assert(!bv2(29))
    assert(!bv2(30))
  }

  it should "set bits from" in {
    val bv = BitVector.cleared(50)
    val bitVector = bv.setFrom(30)

    bitVector should not be theSameInstanceAs(bv)
    bitVector should have('cardinality(20))

    for (i <- 0 until 30) {
      assert(!bitVector(i));
    }
    for (i <- 30 until 50) {
      assert(bitVector(i));
    }
    for (i <- 50 until 100) {
      assert(!bitVector(i));
    }
  }

  it should "clear bits until" in {
    val bv = BitVector.cleared(50) + 26 + 29 + 40
    val bitVector = bv.clearTo(29)

    bitVector should not be theSameInstanceAs(bv)
    bitVector should have('cardinality(2))
    assert(!bitVector(26));
    assert(!bitVector(29));
    assert(bitVector(40));

    val bv2 = bitVector.clearTo(25)
    bv2 should be theSameInstanceAs (bitVector)

  }

  it should "detect subsets" in {
    val bitVector = BitVector.cleared(50) + 26 + 29 + 30

    val bv2 = BitVector.cleared(50) + 26

    assert(!bitVector.subsetOf(bv2));
    assert(bv2.subsetOf(bitVector));
  }

  
  it should "detect equality" in {
    val bitVector = BitVector.cleared(50) + 46
    
    val lbv = BitVector.cleared(128) + 46
    
    bitVector shouldBe lbv
    lbv shouldBe bitVector
    
    val lbv2 = lbv + 100
    bitVector should not be (lbv2)
    lbv2 should not be (bitVector)
  }

}
