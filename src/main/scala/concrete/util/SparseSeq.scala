package concrete.util

import java.util.Arrays

object SparseSeq {
  def apply(a: Array[Int]) = new SparseSeq(a, a.length)
}

class SparseSeq(
    val values: Array[Int],
    override val size: Int) extends Iterable[Int] {

  def this(max: Int) = this(new Array(max), 0)

  override def foreach[U](f: Int => U): Unit = {
    var i = size - 1
    while (i >= 0) {
      f(values(i))
      i -= 1
    }
  }

  def apply(i: Int): Int = values(i)

  def padTo(newSize: Int): SparseSeq = {
    new SparseSeq(values.padTo(newSize, 0), size)
  }

  def copy = new SparseSeq(values.clone, size)

  def +(i: Int): SparseSeq = {
    val nv = Arrays.copyOf(values, size + 1)
    nv(size) = i
    new SparseSeq(nv, size + 1)
  }

  def removeIndex(i: Int): SparseSeq = {
    val newSize = size - 1

    new SparseSeq(values, newSize)
  }

  override def filter(f: Int => Boolean): SparseSeq = {

    var newSize = size
    var i = newSize - 1
    while (i >= 0) {
      val v = values(i)
      if (!f(v)) {
        newSize -= 1
        values(i) = values(newSize)
        values(newSize) = v
      }
      i -= 1
    }
    new SparseSeq(values, newSize)
  }

  override def isEmpty = size == 0

  def iterator = values.iterator.take(size)

}