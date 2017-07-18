package concrete.util

class Buffer(sizeHint: Int = 16) {

  var array = new Array[Int](computeSize(8, sizeHint))
  var last = 0

  def apply(i: Int) = array(i)

  def update(i: Int, v: Int) = {
    last = math.max(last, i + 1)
    array(i) = v
  }

  /** Ensure that the internal array has at least `n` cells. */
  def ensureSize(n: Int) {
    // Use a Long to prevent overflows
    val arrayLength = array.length
    if (n > arrayLength) {
      val newArray: Array[Int] = new Array(computeSize(arrayLength, n))
      java.lang.System.arraycopy(array, 0, newArray, 0, last)
      array = newArray
    }
  }

  private def computeSize(arrayLength: Int, n: Int): Int = {
    var newSize: Long = arrayLength.toLong * 2
    while (n > newSize)
      newSize = newSize * 2
    // Clamp newSize to Int.MaxValue
    if (newSize > Int.MaxValue) newSize = Int.MaxValue
    newSize.toInt
  }

  def applicable(i: Int) = i < array.size

  override def toString = iterator(last).mkString("[", ", ", "]")

  def iterator(members: Int) = array.iterator.take(members)
}

class SparseSetBuffer(
                 private val dense: Buffer = new Buffer(),
                 private val sparse: Buffer = new Buffer(),
                 private val members: Int = 0) extends Set[Int] {

  def this(sizeHint: Int) = this(new Buffer(sizeHint), new Buffer(sizeHint))

  override def apply(k: Int) = contains(k)

  def contains(k: Int) = {
    sparse.applicable(k) && {
      val a = sparse(k)
      a < members && dense(a) == k
    }
  }

  def +(k: Int): SparseSetBuffer = {
    sparse.ensureSize(k + 1)
    val a = sparse(k)
    val b = members
    if (a >= b || dense(a) != k) {
      sparse(k) = b
      dense.ensureSize(b + 1)
      dense(b) = k
      new SparseSetBuffer(dense, sparse, b + 1)
    } else {
      this
    }
  }

  def iterator: Iterator[Int] = dense.iterator(members)

  override def size = members

  def -(elem: Int): SparseSetBuffer = {
    if (sparse.applicable(elem)) {
      val a = sparse(elem)
      if (a < members && dense(a) == elem) {
        val end = members - 1
        swap(elem, a, end)
        new SparseSetBuffer(dense, sparse, end)
      } else {
        this
      }
    } else {
      this
    }
  }

  private def swap(elem: Int, a: Int, end: Int): Unit = {
    val swapped = dense(end)
    dense(a) = swapped
    dense(end) = elem
    sparse(elem) = end
    sparse(swapped) = a
  }

}
