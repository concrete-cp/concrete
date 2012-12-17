package cspfj.util

class BitVectorIterator(bv: BitVector) extends Iterator[Int] {
  var current = bv.nextSetBit(0)
  def hasNext = current >= 0
  def next() = {
    val c = current
    current = bv.nextSetBit(current + 1)
    c
  }
}
