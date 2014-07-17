package concrete.util;


import org.scalatest.Matchers
import org.scalatest.FlatSpec

final class LargeBitVectorTest extends FlatSpec with Matchers {

  "LargeBitVectors" should "be filled" in {
    var bitVector = BitVector.filled(125)
    assert(bitVector(64));
    assert(bitVector(65));
    assert(bitVector(124));
    assert(!bitVector(125));

    bitVector = BitVector.cleared(125)
    bitVector.nextSetBit(0) shouldBe -1
  }

  it should "compute correct number of words" in {
    BitVector.nbWords(0) shouldBe 0
    BitVector.nbWords(1) shouldBe 1
    BitVector.nbWords(64) shouldBe 1
    BitVector.nbWords(65) shouldBe 2
  }

  it should "set bits" in {
    var bitVector = BitVector.cleared(125)
    assert(!bitVector(100))
    bitVector += 100
    assert(bitVector(100))
    bitVector = bitVector.set(100, false)
    assert(!bitVector(100))
  }

  it should "get bits" in {
    val bitVector = BitVector.cleared(125) + 46
    assert(!bitVector(0));
    assert(!bitVector(45));
    assert(bitVector(46));
  }

  it should "compute next bit" in {
    val bitVector = BitVector.cleared(125) + 46 + 49 + 100
    bitVector.nextSetBit(0) shouldBe 46
    bitVector.nextSetBit(46) shouldBe 46
    bitVector.nextSetBit(47) shouldBe 49
    bitVector.nextSetBit(63) shouldBe 100
    bitVector.nextSetBit(64) shouldBe 100
    bitVector.nextSetBit(101) shouldBe -1
  }

  it should "compute prev cleared bit" in {
    var bitVector = BitVector.filled(125) - 46 - 49 - 100

    bitVector.prevClearBit(47) shouldBe 46
    bitVector.prevClearBit(46) shouldBe -1
    bitVector.prevClearBit(45) shouldBe -1
    bitVector.prevClearBit(110) shouldBe 100

    bitVector.prevClearBit(64) shouldBe 49
    bitVector.prevClearBit(63) shouldBe 49

    bitVector -= 64
    bitVector.prevClearBit(65) shouldBe 64
    bitVector.prevClearBit(64) shouldBe 49

    bitVector += 64
    bitVector -= 63
    bitVector.prevClearBit(65) shouldBe 63
    bitVector.prevClearBit(63) shouldBe 49

  }

  it should "compute prev set bit" in {
    var bitVector = BitVector.cleared(125) + 46 + 49 + 100;

    bitVector.prevSetBit(47) shouldBe 46
    bitVector.prevSetBit(46) shouldBe -1
    bitVector.prevSetBit(45) shouldBe -1
    bitVector.prevSetBit(110) shouldBe 100

    bitVector.prevSetBit(64) shouldBe 49
    bitVector.prevSetBit(63) shouldBe 49

    bitVector += 64
    bitVector.prevSetBit(65) shouldBe 64
    bitVector.prevSetBit(64) shouldBe 49

    bitVector -= 64
    bitVector += 63
    bitVector.prevSetBit(65) shouldBe 63
    bitVector.prevSetBit(63) shouldBe 49

  }

  it should "be converted to String" in {
    val bitVector = BitVector.cleared(125) + 46 + 49 + 100
    bitVector.toString shouldBe "{46, 49, 100}"
  }

  it should "compute correct word positions" in {
    BitVector.word(0) shouldBe 0
    BitVector.word(63) shouldBe 0
    BitVector.word(64) shouldBe 1
  }

  it should "correctly clear parts from bit" in {
    val bitVector = BitVector.cleared(125) + 46 + 49 + 100
    bitVector.clearFrom(101) should be theSameInstanceAs(bitVector)
    val bv2 = bitVector.clearFrom(47)
    bv2 should not be theSameInstanceAs(bitVector)
    bv2.cardinality shouldBe 1
    assert(bv2(46))
    assert(!bv2(49))
    assert(!bv2(100))
  }

  it should "correctly clear parts to bit" in {
    val bitVector = BitVector.cleared(125) + 46 + 49 + 100
    bitVector.clearTo(45) should be theSameInstanceAs(bitVector)
    
    val bv2 = bitVector.clearTo(49)
    bv2 should not be theSameInstanceAs (bitVector)
    bv2.cardinality shouldBe 2
    assert(!bv2(46))
    assert(!bv2(49))
    assert(bv2(100))
  }

 it should "correctly set parts from bit" in {
   val bitVector = BitVector.cleared(125)
   val bv2 = bitVector.setFrom(80)
   
   bv2 should not be theSameInstanceAs(bitVector)
    
   bitVector.cardinality shouldBe 45
   
    for (i <- 0 until 80) {
      assert(!bitVector(i));
    }
    for (i <- 80 until 125) {
      assert(bitVector(i));
    }
    for (i <- 125 until 200) {
      assert(!bitVector(i));
    }

    val bv = BitVector.filled(2000)
    bv.setFrom(100) should be theSameInstanceAs(bv)
  }

 it should "detect subsets" in {
   val bitVector = BitVector.cleared(125) + 46 + 49 + 100

    val bv2 = BitVector.cleared(125) + 46
    
    assert(!bitVector.subsetOf(bv2));
    assert(bv2.subsetOf(bitVector));

    var bv3 = BitVector.cleared(70);
    var bv4 = BitVector.cleared(70);
    for (i <- 0 until 70) {
      bv3 += i;
      bv4 += i;
    }
    assert(bv3.subsetOf(bv4));
    assert(bv4.subsetOf(bv3));
    bv3 shouldBe bv4
  }

 it should "have correct hash codes" in {
    val bitVector = BitVector.cleared(125) + 46
    
    val bv2 = BitVector.cleared(400) + 46
    

    val bv3 = BitVector.cleared(50) + 46

    bitVector shouldBe bv2
    bitVector shouldBe bv3
    bitVector.hashCode shouldBe bv2.hashCode
    bitVector.hashCode shouldBe bv3.hashCode

  }

  it should "compute xor" in {
    val bitVector = BitVector.cleared(125) + 59 + 11
    
    val bv2 = BitVector.cleared(20) + 10 +11

    val bv3 = bitVector ^ bv2
    val bv4 = bv2 ^ bitVector
    
    bv3 shouldBe bv4
    bv2.cardinality shouldBe 2
    
    assert(bv3(10))
    assert(!bv3(11))
    assert(bv3(59))

  }

}
