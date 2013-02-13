package cspfj.util

class SparseSet(
  private val dense: Array[Int],
  private val sparse: Array[Int],
  private var members: Int) {

  def this(size: Int) = this(new Array[Int](size), new Array[Int](size), 0)

  def apply(k: Int) = {
    val a = sparse(k)
    a < members && dense(a) == k
  }

  def +=(k: Int) {
    val a = sparse(k)
    val b = members
    if (a >= b || dense(a) != k) {
      sparse(k) = b
      dense(b) = k
      members = b + 1
    }
  }

  def save = new SparseSet(dense, sparse, members)

}