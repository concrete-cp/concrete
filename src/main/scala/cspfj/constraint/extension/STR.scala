package cspfj.constraint.extension

import cspfj.util.BitVector

class STR(array: Array[Array[Int]], val bound: Int) extends Relation {
  type Self2 = STR

  def this() = this(Array(), 0)

  def copy = new STR(array.clone, bound)

  def +(t: Array[Int]) = new STR(t +: array, bound + 1)

  override def ++(t: Iterable[Array[Int]]) = new STR(t ++: array, bound + t.size)

  def -(t: Array[Int]) = throw new UnsupportedOperationException

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]) = {
    var b = bound
    var i = 0
    while (i < b) {
      if (modified.forall { p => f(p, array(i)(p)) }) i += 1
      else {
        b -= 1
        val tmp = array(i)
        array(i) = array(b)
        array(b) = tmp
      }
    }
    if (b == bound) this
    else new STR(array, b)
  }

  def fillFound(f: (Int, Int) => Boolean, arity: Int) = {
    val pos = new MutableList(arity)
    var i = bound - 1
    while (i >= 0) {
      val tuple = array(i)

      var pi = 0
      while (pi < pos.nb) {
        val p = pos(pi)

        if (f(p, tuple(p))) pos.remove(pi) else pi += 1
      }
      i -= 1
    }
    pos
  }

  //  def fillFound(f: (Int, Int) => Boolean, arity: Int) = {
  //    var i = bound - 1
  //    while (i >= 0) {
  //      val tuple = array(i)
  //
  //      var pi = arity - 1
  //      while (pi >= 0) {
  //        f(pi, tuple(pi))
  //        pi -= 1
  //      }
  //      i -= 1
  //    }
  //    0 until arity
  //  }

  def nodes = bound * array(0).length

  def find(f: (Int, Int) => Boolean) = throw new UnsupportedOperationException

  def iterator = array.iterator.take(bound)

  def contains(t: Array[Int]) = throw new UnsupportedOperationException

  override def size = bound
}

final class MutableList(size: Int) extends Traversable[Int] {
  private val data = {
    val d = new Array[Int](size)
    var i = size - 1
    while (i >= 0) {
      d(i) = i
      i -= 1
    }
    d
  }

  private var _nb = size

  def apply(index: Int) = data(index)

  def remove(index: Int) {
    _nb -= 1
    data(index) = data(_nb)
  }

  def nb = _nb

  def foreach[U](f: Int => U) {
    var i = _nb - 1
    while (i >= 0) {
      f(data(i))
      i -= 1
    }
  }

}
