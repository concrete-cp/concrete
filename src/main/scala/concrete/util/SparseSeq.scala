package concrete.util

import scala.collection.mutable.ArrayBuffer

object SparseSeq {
  def apply[A](a: A*) = new SparseSeq(a.to[ArrayBuffer], a.length)
}

class SparseSeq[A](
    val values: ArrayBuffer[A],
    override val size: Int) extends Iterable[A] {

  def this() = this(new ArrayBuffer, 0)

  override def foreach[U](f: A => U): Unit = {
    var i = size - 1
    while (i >= 0) {
      f(values(i))
      i -= 1
    }
  }

  def apply(i: Int): A = values(i)

  def copy = new SparseSeq(values.clone, size)

  def +(i: A): SparseSeq[A] = {
    values += i
    new SparseSeq(values, size + 1)
  }

  override def filter(f: A => Boolean): SparseSeq[A] = {

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