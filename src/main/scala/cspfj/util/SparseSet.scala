package cspfj.util

import java.util.Arrays

class SparseSet(
  private val dense: Array[Int],
  private val sparse: Array[Int],
  private val members: Int) extends Set[Int] {

  def this() = this(new Array[Int](10), new Array[Int](10), 0)

  private def ensureCapacity(minCapacity: Int): SparseSet = {
    val oldCapacity = sparse.length;

    if (minCapacity > oldCapacity) {
      val newCapacity = math.max(minCapacity, (oldCapacity * 3) / 2 + 1);
      new SparseSet(
        Arrays.copyOf(dense, newCapacity),
        Arrays.copyOf(sparse, newCapacity),
        members)
    } else {
      this
    }

  }

  def contains(k: Int) = {
    k < sparse.length && {
      val a = sparse(k)
      a < members && dense(a) == k
    }
  }

  def +(k: Int) = ensureCapacity(k + 1).addCap(k)

  private def addCap(k: Int): SparseSet = {
    val a = sparse(k)
    val b = members
    if (a >= b || dense(a) != k) {
      sparse(k) = b
      dense(b) = k
      new SparseSet(sparse, dense, b + 1)
    } else {
      this
    }
  }

  // Members declared in scala.collection.GenSetLike 
  def iterator: Iterator[Int] = ???
  // Members  declared in scala.collection.SetLike 
  def -(elem: Int): scala.collection.immutable.Set[Int] = ???

}
