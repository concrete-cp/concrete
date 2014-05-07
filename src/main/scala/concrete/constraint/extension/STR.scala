package concrete.constraint.extension

import concrete.util.BitVector
import java.util.Arrays
import scala.annotation.tailrec
import concrete.Variable

object STR extends RelationGenerator {
  def apply(data: Seq[Array[Int]]): STR = {
    val d = data.toArray
    new STR(d, d.length)
  }
}

final class STR(val array: Array[Array[Int]], var bound: Int) extends Relation {
  type Self2 = STR

  def this() = this(Array(), 0)

  def copy = new STR(array.clone, bound)

  def +(t: Array[Int]) = new STR(t +: array, bound + 1)

  override def ++(t: Iterable[Array[Int]]) = new STR(t ++: array, bound + t.size)

  def -(t: Array[Int]) = throw new UnsupportedOperationException

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]) = {
    var b = bound
    var i = b - 1
    while (i >= 0) {
      if (!valid(modified, f, i)) {
        b -= 1
        val tmp = array(i)
        array(i) = array(b)
        array(b) = tmp
      }
      i -= 1
    }
    if (b == bound) {
      this
    } else {
      new STR(array, b)
    }
  }

  @tailrec
  private def valid(modified: List[Int], f: (Int, Int) => Boolean, i: Int): Boolean = modified match {
    case Nil => true
    case h :: t => f(h, array(i)(h)) && valid(t, f, i)
  }

  private var pos: MutableList = _

  def fillFound(f: (Int, Int) => Boolean, arity: Int) = {
    try {
      pos.refill()
    } catch {
      case e: NullPointerException =>
        pos = new MutableList(arity)
        pos.refill()
    }
    var i = bound - 1
    while (i >= 0) {
      val tuple = array(i)
      var pi = pos.nb - 1
      while (pi >= 0) {
        val p = pos(pi)
        if (f(p, tuple(p))) pos.remove(pi)
        pi -= 1
      }
      i -= 1
    }
    pos.sorted
  }

  override def toString = s"$bound of ${array.size} tuples"

  def edges = if (array.isEmpty) 0 else bound * array(0).length

  def find(f: (Int, Int) => Boolean) = throw new UnsupportedOperationException
  def findSupport(scope: Array[Variable], p: Int, i: Int, support: Array[Int]) = throw new UnsupportedOperationException

  def iterator = array.iterator.take(bound)

  def contains(t: Array[Int]): Boolean = throw new UnsupportedOperationException
  //  {
  //    val i = bound - 1
  //    while (i >= 0) {
  //      if (Arrays.equals(t, array(i))) return true
  //    }
  //    false
  //  }

  override def size = bound
  def lambda = bound
}

final class MutableList(size: Int) extends Traversable[Int] {
  private val data = new Array[Int](size)

  def refill() {
    var i = size - 1
    while (i >= 0) {
      data(i) = i
      i -= 1
    }
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

  def sorted = {
    val a = toArray
    Arrays.sort(a)
    a.reverse
  }

}
