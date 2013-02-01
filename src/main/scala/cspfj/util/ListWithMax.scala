package cspfj.util

final class ListWithMax(length: Int) extends Traversable[Int] {
  var max = length - 1
  var candidates = BitVector.newBitVector(length)
  candidates.fill(true)

  def clear(i: Int) {
    if (candidates.clear(i) && i == max) {
      max = candidates.prevSetBit(i)
    }
  }

  def foreach[U](f: Int => U) {
    var i = candidates.nextSetBit(0)
    while (i >= 0) {
      f(i)
      i = candidates.nextSetBit(i + 1)
    }
  }
}