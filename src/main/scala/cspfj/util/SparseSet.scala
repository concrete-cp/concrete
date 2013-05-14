package cspfj.util

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

  // Members declared in scala.collection.GenSetLike
  def iterator: Iterator[Int] = new Iterator[Int] {
    var c = members - 1
    def hasNext = c >= 0
    def next() = {
      val v = sparse(dense(c))
      c -= 1
      v
    }
  }

  override def size = members

  // Members  declared in scala.collection.SetLike 
  def -(elem: Int): scala.collection.immutable.Set[Int] = ???

}
