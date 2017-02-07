package concrete.util

import bitvectors.BitVector

final class SetWithMax(length: Int) extends collection.mutable.Set[Int] {
  var max = length - 1
  var candidates = BitVector.filled(length)

  def +=(i: Int) = ???

  def -=(i: Int) = {
    val oldC = candidates
    candidates -= i

    if (i == max) {
      max = candidates.prevSetBit(max + 1)
    }
    //println(s"- $i -> $this")
    this
  }

  def clearFrom(newMax: Int): Unit = {
    if (newMax <= max) {
      max = candidates.prevSetBit(newMax)
    }
    //println(s"clearFrom($newMax) -> $this")
  }

  override def foreach[U](f: Int => U) {
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
        this -= i
      }
      i = candidates.prevSetBit(i)
    }
    SetWithMax.this
  }

  def iterator: Iterator[Int] = new Iterator[Int] {
    var i = max
    def hasNext = (i >= 0)
    def next() = {
      val c = i
      i = candidates.prevSetBit(i)
      c
    }
  }

  def contains(elem: Int): Boolean = elem <= max && candidates(elem)

}
