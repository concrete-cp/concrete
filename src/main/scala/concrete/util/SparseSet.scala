package concrete.util

import java.util.Arrays

class SparseSet(
    val dense: Array[Int],
    private val sparse: Array[Int],
    private val members: Int) extends Set[Int] {

  def this(s: Int) = this(new Array[Int](s), new Array[Int](s), 0)

  def contains(k: Int) = {

    val a = sparse(k)
    a < members && dense(a) == k

  }

  def +(k: Int): SparseSet = {
    val a = sparse(k)
    val b = members
    if (a >= b || dense(a) != k) {
      sparse(k) = b
      dense(b) = k
      new SparseSet(dense, sparse, b + 1)
    } else {
      this
    }
  }

  def iterator: Iterator[Int] = dense.iterator.take(members)

  override def size = members

  private def swap(elem: Int, a: Int, end: Int): Unit = {
    val t = dense(a)
    dense(a) = dense(end)
    dense(end) = t
    sparse(elem) = end
    sparse(end) = a

  }

  def -(elem: Int): SparseSet = {
    val a = sparse(elem)
    if (a < members && dense(a) == elem) {
      val end = members - 1
      swap(elem, a, end)
      new SparseSet(dense, sparse, end)
    } else {
      this
    }

  }

}
