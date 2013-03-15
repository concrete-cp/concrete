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
    var i = max
    while (i >= 0) {
      f(i)
      i = candidates.prevSetBit(i)
    }
  }

  override def filter(f: Int => Boolean) = {
    var i = max
    while (i >= 0) {
      if (!f(i)) {
        clear(i)
      }
      i = candidates.prevSetBit(i)
    }
    this
  }
}