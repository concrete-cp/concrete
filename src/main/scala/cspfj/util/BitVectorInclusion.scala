package cspfj.util

final class BitVectorInclusion extends PredefPO[BitVector] with EnhancedPartialOrdering[BitVector] {
  def lteq(a: BitVector, b: BitVector) = a.subsetOf(b)

  override def lt(a: BitVector, b: BitVector) = lteq(a, b) && !lteq(b, a)

  def disjoint(a: BitVector, b: BitVector) = a.intersects(b) >= 0
}