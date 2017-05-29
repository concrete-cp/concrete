package concrete.util

class SparseSet(
    private val dense: Array[Int],
    private val sparse: Array[Int],
    private val members: Int) extends Set[Int] {

  def this(capacity: Int) = this(new Array[Int](capacity), new Array[Int](capacity), 0)

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


  def capacity = sparse.length
  
  def iterator: Iterator[Int] = dense.iterator.take(members)

  override def size = members

  private def swap(elem: Int, a: Int, end: Int): Unit = {
    val swapped = dense(end)
    dense(a) = swapped
    dense(end) = elem
    sparse(elem) = end
    sparse(swapped) = a
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
